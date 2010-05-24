{-# OPTIONS -fno-warn-incomplete-record-updates #-}

-- | Basic utils for working with the type graph.
module Type.Class
	( classChildren
	, expandGraph
	, allocClass
	, delClass
	, makeClassFromVar
	, addToClass
	, lookupClass
	, updateClass
	, modifyClass
	, mergeClasses

	, lookupVarToClassId
	, makeClassName
	, clearActive
	, activateClass
	, sinkVar
	, kindOfCid
	, foldClasses
	, lookupSourceOfNode
	, deleteSingleFetter
	, takeTClassOfClass

	-- * Sinking
	, sinkClassId
	, sinkCidsInNode
	, sinkCidsInType
	, sinkCidsInFetter
	, sinkCidsInNodeFst
	, sinkCidsInFetterFst)
where
import Type.Exp
import Type.Location
import Type.State
import Type.Plate.Collect
import Type.Util
import Type.Dump		()
import Util
import DDC.Main.Error
import DDC.Solve.Sink
import DDC.Var
import Data.Array.IO
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import qualified Data.Sequence	as Seq

debug	= False
trace s	= when debug $ traceM s
stage	= "Type.Squid.Class"

-- | Return the cids of all the children of this class.
classChildren :: Class -> [ClassId]
classChildren c
 = case c of
 	ClassFetter { classFetter = f }	
	 -> Set.toList $ collectClassIds f

	Class	    { classType   = mt } 
	 -> fromMaybe [] $ liftM (Set.toList . cidsOfNode) mt


-- | Increase the size of the type graph.
expandGraph 
	:: Int 			-- Graph must have this many free nodes after expansion.
	-> SquidM ()

expandGraph minFree
 = do	graph			<- gets stateGraph
 	ClassId curMax		<- liftM snd $ liftIO $ getBounds (graphClass graph)
	let curIx		= graphClassIdGen graph
	
	if curIx + minFree <= curMax
	 then return ()
	 else do 
	 	let newMax	= curMax * 2

		elems		<- liftIO (getElems (graphClass graph))
		newClass	<- liftIO 
					(newListArray (ClassId 0, ClassId newMax)
					(elems ++ replicate curMax ClassUnallocated))

		let graph'	= graph
				{ graphClass	= newClass }

		modify (\s -> s { stateGraph = graph' })
		return ()
 	


-- | Allocate a new class in the type graph.
allocClass 	
	:: Kind			-- The kind of the class.
	-> SquidM ClassId

allocClass kind
 = do	expandGraph	1

	graph		<- gets stateGraph
	let classIdGen	=  graphClassIdGen graph
 	let cid		= ClassId classIdGen

	liftIO 	$ writeArray (graphClass graph) cid
		$ classInit cid kind

	let graph'	= graph
			{ graphClassIdGen	= classIdGen + 1}

	modify (\s -> s { stateGraph		= graph' })
	return cid


-- | Delete a class by setting it to Nil.
--   Note: This class *cannot* be re-used because it may have been deleted due to a MPTC
--	   being crushed out. Other nodes will still refer to this one, and Type.Trace 
--	   treats the ClassFetterDeleted as generating no constraints.
delClass :: ClassId -> SquidM ()
delClass cid
 = do	Just cls	<- lookupClass cid
	case cls of
	 ClassFetter{}	
	  -> do	updateClass cid (ClassFetterDeleted cls)
		return ()
		
	 _ ->	panic stage $ "delClass: class " % cid % " to be deleted is not a ClassFetter{}"
	
	
-- | If there is already a class for this variable then return that
--		otherwise make a new one containing this var.
makeClassFromVar
	:: TypeSource		-- ^ Source of the constraint containing the variable.
	-> Kind			-- ^ Kind of this variable.
	-> Var			-- ^ The variable.
	-> SquidM ClassId
		
makeClassFromVar tSource kind var
 = do	mCid		<- lookupVarToClassId var
   	case mCid of
   	 Just cid	-> return cid
	 Nothing 
	  -> do	cid	<- allocClass kind
		addNameToClass cid tSource var kind
	     	return	cid


-- | Return a variable to identify this class.
--	If the class already contains variables then choose the one with the smallest display name
--	otherwise make a new variable and use that.
--	
makeClassName :: ClassId -> SquidM Var
makeClassName cid_
 = do
 	cid		<- sinkClassId cid_
	Just c		<- lookupClass cid

	let kind	= case c of { Class { classKind = kind } -> kind }
	let vars	= [ v | (NVar v, _) <- classTypeSources c ]	
			
	case vars of
	 [] 
	  -> case spaceOfKind $ resultKind kind of
		Nothing	
		 -> panic stage $ "no space for kind " % kind % "\n"

		Just nameSpace
		 -> do	v	<- newVarN nameSpace
			let tSource	= TSI $ SIClassName
			addNameToClass cid tSource v kind
			return	v
		
	 (_:_)
	  -> do
	  	let v	= head $ sortBy classNameOrd vars
		return v
			
classNameOrd v1 v2

	| length (varName v1) < length (varName v2)
	= Prelude.LT
	
	| length (varName v1) > length (varName v2)
	= Prelude.GT
	
	| otherwise
	= Prelude.EQ


-- | Add a new type constraint to a class
--	Doing this makes the class active.
addToClass 
	:: ClassId		-- ^ id of class to update
	-> TypeSource		-- ^ source of constraint
	-> Kind			-- ^ kind of the constraint
	-> Node			-- ^ constraint
	-> SquidM ()

addToClass cid src kind node
 = do	graph		<- gets stateGraph
	addToClass2 cid src kind node graph

addToClass2 cid' src kind node graph
 = go cid'
 where	go cid
	 = do	cls	<- liftIO (readArray (graphClass graph) cid)
		case cls of
		 ClassForward _ cid'' 	-> go cid''
		 ClassUnallocated	-> update cid (classInit cid kind)
		 Class{}		-> update cid cls
		 	
	update cid cls@Class{}
	 = do	liftIO 	$ writeArray (graphClass graph) cid 
			$ cls 	{ classType		= Nothing
				, classTypeSources	= (node, src) : classTypeSources cls }
			
		activateClass cid
		linkVar cid node
		


linkVar cid tt
 = case tt of
 	NVar v
	 -> do	graph	<- gets stateGraph
		let graph'	
			= graph 
			{ graphVarToClassId = Map.insert v cid (graphVarToClassId graph) }
		modify $ \s -> s { stateGraph = graph' }

	_ -> return ()


-- | This is like addToClass, except that if we just give a class a new name
--	then we don't need to change its type
addNameToClass 
	:: ClassId
	-> TypeSource
	-> Var
	-> Kind
	-> SquidM ()

addNameToClass cid_ src v kind
 = do	let node	= NVar v
 	graph	<- gets stateGraph
 	cid	<- sinkClassId cid_
 	cls	<- liftIO (readArray (graphClass graph) cid)
	cls'	<- addNameToClass2 cid src node kind cls
	liftIO (writeArray (graphClass graph) cid cls')
	linkVar cid node
	return ()

addNameToClass2 cid src node kind cls
 = case cls of
 	ClassUnallocated
	 -> addNameToClass3 cid src node (classInit cid kind)

	Class{}		
	 -> addNameToClass3 cid src node cls
	
addNameToClass3 cid src node cls
 = do 	activateClass cid
 	return	$ cls
		{ classTypeSources = (node, src) : classTypeSources cls }


-- | Lookup a class from the graph.
lookupClass 
	:: ClassId 
	-> SquidM (Maybe Class)

lookupClass cid_ 
 = do	cid	<- sinkClassId cid_
	graph	<- gets stateGraph
	c	<- liftIO (readArray (graphClass graph) cid)
	return $ Just c



-- | Update a class in the graph.
updateClass	
	:: ClassId 		-- ^ id of class to update.
	-> Class  		-- ^ new class.
	-> SquidM ()

updateClass cid_ c
 = do	cid		<- sinkClassId cid_
 	graph		<- gets stateGraph
	liftIO (writeArray (graphClass graph) cid c)
	return ()


-- | Modify a class in the graph using this modification function
modifyClass
	:: ClassId
	-> (Class -> Class)
	-> SquidM ()
	
modifyClass cid_ f
 = do	cid	<- sinkClassId cid_
 	graph	<- gets stateGraph
	c	<- liftIO (readArray (graphClass graph) cid)
	liftIO (writeArray (graphClass graph) cid (f c))
	return ()
	
	
-- | Add a forwards to this class.
addClassForwards 
	:: ClassId 		-- class to point to.
	-> [ClassId] 		-- classes to point from.
	-> SquidM ()

addClassForwards cidL_ cids_
 = do	
	-- sink the cids.
 	cidL		<- sinkClassId cidL_
	cids		<- mapM (\cid -> sinkClassId cid) cids_

	-- add a substitution for each elem of cids.
	graph		<- gets stateGraph

	mapM_		(\x -> liftIO (writeArray (graphClass graph) x (ClassForward x cidL))) 
			cids
	
	return ()
	
	
-- | Lookup the variable name of this class.
lookupVarToClassId :: 	Var -> SquidM (Maybe ClassId)
lookupVarToClassId v
 = do	graph		<- gets stateGraph
 	let vMap	= graphVarToClassId graph

	case Map.lookup v vMap of
	 Nothing	-> return Nothing
	 Just cid	
	  -> do	cid'	<- sinkClassId cid
	  	return	$ Just cid'
	 
	
-- Merge ------------------------------------------------------------------------------------------	 
-- | Merge two classes by concatenating their queue and node list
--	The one with the lowesed classId gets all the constraints and the others 
--	are updated to be ClassFowards which point to it.
mergeClasses
	:: [ClassId] 
	-> SquidM ClassId

-- if there's just a single cids then there's nothing to do
mergeClasses [cid_]
 = do	cid'		<- sinkClassId cid_
   	return	cid'
	
mergeClasses cids_
 = do	
	-- Sink the cids and lookup their classes.
 	cids	<- liftM nub $ mapM sinkClassId cids_
	Just cs	<- liftM sequence  $ mapM lookupClass cids
			
	-- Make sure all the classes have the same kind	
	let ks	= map (\c@Class { classKind } -> classKind) cs
	
	case nub ks of
	 [k]	-> mergeClasses2 cids cs 
	 _	-> panic stage
			$ "mergeClasses: classes have differing kinds\n"
			% "    cids = " % cids 	% "\n"
			% "    ks   = " % ks	% "\n"
			% "    cs\n"	% vcat cs
	
mergeClasses2 cids cs
 = do	-- The class with the lowest cid gets all the items.
	let Just cidL	= takeMinimum cids
	Just cL		<- lookupClass cidL
	
	trace 	$ "-- mergeClasses\n"
		% "    cidL = " % cidL % "\n"
		% "    cids = " % cids % "\n"

	let cL'	= cL 	
		{ classType		= Nothing
		, classTypeSources	= concatMap classTypeSources cs
		, classFetters		= Map.unionsWith (Seq.><) $ map classFetters cs
		, classFettersMulti	= Set.unions $ map classFettersMulti cs  }

	updateClass cidL cL'
	activateClass cidL

	-- Add forwards from old classes to new class.
	let cidsH	= cids \\ [cidL]
	addClassForwards cidL cidsH
	
  	return	$ cidL
		

-- | Clear the set of active classes.
clearActive ::	SquidM (Set ClassId)
clearActive
 = do	graph	<- gets stateGraph 
	
	active'	<- liftM Set.fromList
		$  mapM sinkClassId 
		$  Set.toList
		$  graphActive graph
	
	let graph'	= graph { graphActive = Set.empty }
	
	modify $ \s -> s { stateGraph	= graph' }			
	return	active'


-- | Activate a class, tagging it for inspection by the unifier \/ crusher.
--	Also activate any MPTCs acting on it.
activateClass :: ClassId -> SquidM ()
activateClass cid
 = do	-- traceM $ "activating class " % cid % "\n"

	graph		<- gets stateGraph
 	let graph'	= graph { graphActive	= Set.insert cid (graphActive graph) }

	modify $ \s -> s { 
		stateGraph = graph' }

	Just c		<- lookupClass cid
	(case c of
		Class { classFettersMulti = cidsMulti}
		 	-> mapM_ activateClass $ Set.toList cidsMulti
		_	-> return ())
		

-- lookup the kind of the class corresponding to this var.
kindOfCid :: ClassId -> SquidM Kind
kindOfCid cid
 = do	Just c	<- lookupClass cid
 	return	$ classKind c


-- Fold a function through all the classes in the type graph.
foldClasses :: (a -> Class -> SquidM a) -> a -> SquidM a
foldClasses fun x
 = do  	graph		<- gets stateGraph
	classes		<- liftIO $ getElems $ graphClass graph
	foldM fun x classes  


-- | Get the source of some effect, given the class that contains it.
--	The cids in the provided effect must be in canonical form, 
--	but the cids in the class don't need to be.
--	If there are multiple sources in the class then just take the first one.
lookupSourceOfNode
	:: Node
	-> Class 
	-> SquidM (Maybe TypeSource)

lookupSourceOfNode nEff cls
 = do	tsSrcs	<- mapM sinkCidsInNodeFst $ classTypeSources cls
	return 	$ listToMaybe
		$ [nodeSrc	| (nodeEff,  nodeSrc)	<- tsSrcs
				, nodeEff == nEff]


-- | Delete a SPTC Fetter from a class.
deleteSingleFetter
	:: ClassId 
	-> Var
	-> SquidM ()
	
deleteSingleFetter cid v
 = do	Just cls	<- lookupClass cid
	let cls'	= cls { classFetters = Map.delete v (classFetters cls) }
	updateClass cid cls'
	

-- | Turn a Class into a TClass with the same kind and cid
takeTClassOfClass :: Class -> Maybe Type
takeTClassOfClass cls
 = case cls of
	Class{}	-> Just $ TClass (classKind cls) (classId cls)
	_	-> Nothing

	
-- Sinking ----------------------------------------------------------------------------------------

-- | Convert a var to canonical form
sinkVar :: Var -> SquidM Var
sinkVar	var
 = do	mCid	<- lookupVarToClassId var
	case mCid of
	 Nothing	-> return var
	 Just cid	-> makeClassName cid
		

-- | Convert this cid to canconical form.
{-# INLINE sinkClassId #-}
sinkClassId ::	ClassId -> SquidM ClassId
sinkClassId  cid	
 = do	graph		<- gets stateGraph
 	let classes	=  graphClass graph
	sinkClassId' classes cid
	
sinkClassId' classes cid
 = do	mClass	<- liftIO (readArray classes cid)
 	case mClass of
		ClassForward _ cid'	-> sinkClassId' classes cid'
		ClassUnallocated{}	-> panic stage $ "sinkClassId': class is unallocated"
		ClassFetter{}		-> return cid
		ClassFetterDeleted{}	-> return cid
		Class{}			-> return cid


-- | Convert the cids in this node type to canonical form.
sinkCidsInNode :: Node -> SquidM Node
sinkCidsInNode nn
 = do	graph		<- gets stateGraph
	let classes	= graphClass graph
	liftIO $ sinkCidsInNodeIO classes nn


-- | Convert the cids in this type to canonical form.
sinkCidsInType :: Type -> SquidM Type
sinkCidsInType tt
 = do	graph		<- gets stateGraph
	let classes	= graphClass graph
	liftIO $ sinkCidsInTypeIO classes tt


-- | Convert the cids in this type to canonical form.
sinkCidsInFetter :: Fetter -> SquidM Fetter
sinkCidsInFetter ff
 = do	graph		<- gets stateGraph
	let classes	= graphClass graph
	liftIO $ sinkCidsInFetterIO classes ff


-- | Convert the cids in the first element of this tuple to canonical form.
--	Good for the classTypeSources field of a class.
sinkCidsInNodeFst :: (Node, a) -> SquidM (Node, a)	
sinkCidsInNodeFst (nn, x)
 = do	nn'	<- sinkCidsInNode nn
	return	$ (nn', x)


-- | Convert the cids in the first element of this tuple to canonical form.
--	Good for the classFetterSource field of a class.
sinkCidsInFetterFst :: (Fetter, a) -> SquidM (Fetter, a)
sinkCidsInFetterFst (ff, x)
 = do	ff'	<- sinkCidsInFetter ff
	return	$ (ff', x)



