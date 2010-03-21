{-# OPTIONS -fno-warn-incomplete-record-updates #-}

-- | Basic utils for working with the type graph.
module Type.Class
	( classChildren
	, expandGraph
	, allocClass
	, delClass
	, makeClassV
	, addToClass
	, lookupClass
	, updateClass
	, modifyClass
	, mergeClasses
	, mergeClassesT
	, sinkClassId
	, lookupVarToClassId
	, makeClassName
	, clearActive
	, activateClass
	, sinkVar
	, updateVC
	, kindOfCid
	, foldClasses
	, headTypeDownLeftSpine
	, traceDownLeftSpine)
where
import Type.Exp
import Type.Location
import Type.Plate.Trans
import Type.State
import Type.Plate.Collect
import Type.Util
import Util
import Shared.Error
import Data.Array.IO
import Shared.Var		(Var)
import qualified Shared.Var	as Var
import qualified Data.Map	as Map
import qualified Data.Set	as Set


-----
stage	= "Type.Squid.Class"

-- | Return the cids of all the children of this class.
classChildren :: Class -> [ClassId]
classChildren c
 = case c of
 	ClassFetter { classFetter = f }	
	 -> Set.toList $ collectClassIds f

	Class	    { classType   = mt } 
	 -> fromMaybe [] $ liftM (Set.toList . collectClassIds) mt


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
	 	let newMax	= curMax + graphSize_inc

		elems		<- liftIO (getElems (graphClass graph))
		newClass	<- liftIO (newListArray (ClassId 0, ClassId newMax)
						(elems ++ replicate graphSize_inc ClassNil))

		let graph'	= graph
				{ graphClass	= newClass }

		modify (\s -> s { stateGraph = graph' })
		return ()
 	


-- | Allocate a new class in the type graph.
allocClass 	
	:: Maybe Kind			-- The type of the graph.
	-> SquidM ClassId

allocClass mKind
 = do	expandGraph	1

	graph		<- gets stateGraph
	let classIdGen	=  graphClassIdGen graph
 	let cid		= ClassId classIdGen

	let initial
		= case mKind of
			Just kind	-> classInit cid kind
			Nothing		-> ClassNil

	liftIO (writeArray (graphClass graph) cid initial)

	let graph'	= graph
			{ graphClassIdGen	= classIdGen + 1}

	modify (\s -> s { stateGraph		= graph' })
	
	return cid


-- | Delete a class by setting it to Nil.
--	Note: 	This class *cannot* be re-used because it may have been deleted due to a MPTC
--		being crushed out. Other nodes will still refer to this one, and Type.Trace 
--		treats the ClassNil as generating no constraints.
--
delClass :: ClassId -> SquidM ()
delClass cid
 = do	Just c		<- lookupClass cid
	updateClass cid ClassNil
	return ()	
	
	
-- | If there is already a class for this variable then return that
--		otherwise make a new one containing this var.
makeClassV	
	:: TypeSource		-- ^ Source of the constraint containing the variable.
	-> Kind			-- ^ Kind of this variable.
	-> Var			-- ^ The variable.
	-> SquidM ClassId
		
makeClassV tSource kind v
 = do	mCid		<- lookupVarToClassId v

   	case mCid of
   	 Just cid	-> return cid
	 Nothing 
	  -> do	cid	<- allocClass (Just kind)
		addNameToClass cid tSource v kind
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
	let vars	= [ v | (TVar k v, _) <- classTypeSources c ]	
			
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

	| length (Var.name v1) < length (Var.name v2)
	= Prelude.LT
	
	| length (Var.name v1) > length (Var.name v2)
	= Prelude.GT
	
	| otherwise
	= Prelude.EQ


-- | Add a new type constraint to a class
--	Doing this makes the class active.
addToClass 
	:: ClassId		-- ^ id of class to update
	-> TypeSource		-- ^ source of constraint
	-> Type			-- ^ constraint
	-> SquidM ()

addToClass cid_ src t
 = do	graph		<- gets stateGraph
 	cid		<- sinkClassId cid_

 	c		<- liftIO (readArray (graphClass graph) cid)
	c'		<- addToClass2 cid src t c
	liftIO (writeArray (graphClass graph) cid c')

	linkVar cid t

	return ()

addToClass2 cid src t c
	| ClassNil	<- c
	, Just k	<- kindOfType t
	= addToClass3 cid src t (classInit cid k)
		
	| Class { classKind } <- c
	, Just k	<- kindOfType t
	, k == classKind
	= addToClass3 cid src t c
	
	| otherwise
	= panic stage 
		$ "addToClass2 fails: " ++ show t ++ "\n"
		++ "class = " ++ show c ++ "\n"
	

addToClass3 cid src t c@Class{}
 = do 	activateClass cid
 	return	$ c	{ classTypeSources	= (t, src) : classTypeSources c
			, classType		= Nothing
		  	, classQueue		= (maybeToList $ classType c) ++ classQueue c ++ [t] }

linkVar cid tt
 = case tt of
 	TVar k v
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

addNameToClass cid_ src v k
 = do	let t	= TVar k v
 	graph	<- gets stateGraph
 	cid	<- sinkClassId cid_

 	c	<- liftIO (readArray (graphClass graph) cid)
	c'	<- addNameToClass2 cid src t c
	liftIO (writeArray (graphClass graph) cid c')
	linkVar cid t
	return ()

addNameToClass2 cid src t c
 = case c of
 	ClassNil	
	 -> let Just k	= kindOfType t
	    in  addNameToClass3 cid src t (classInit cid k)

	Class{}		
	 -> addNameToClass3 cid src t c
	
addNameToClass3 cid src t c
 = do 	activateClass cid
 	return	$ c	{ classTypeSources	= (t, src) : classTypeSources c }


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

	mapM_		(\x -> liftIO (writeArray (graphClass graph) x (ClassForward cidL))) 
			cids
	
	return ()
	
	

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
		ClassForward cid'	-> sinkClassId' classes cid'
		ClassNil{}		-> return cid
		ClassFetter{}		-> return cid
		Class{}			-> return cid
		

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
	
mergeClasses2 cids cs
 = do	-- The class with the lowest cid gets all the items.
	let Just cidL	= takeMinimum cids
	Just cL		<- lookupClass cidL

	let cL'	= cL 	
		{ classTypeSources	= concat $ map classTypeSources cs
		, classType		= Nothing

		, classQueue	=  nub
				$  catMaybes
				$  map (\t -> case t of
						TBot _	-> Nothing
						_	-> Just t)
				$  concat (map classQueue cs)
				++ concat (map (maybeToList . classType) cs)

		, classFetterSources
			= concat $ map classFetterSources cs

		, classFettersMulti
			= Set.unions $ map classFettersMulti cs  }

	updateClass cidL cL'
	activateClass cidL

	-- Add forwards from old classes to new class.
	let cidsH	= cids \\ [cidL]
	addClassForwards cidL cidsH
	
  	return	$ cidL
		
		
-- | Merge these classes.
--	args must all be TClasses
mergeClassesT :: [Type] -> SquidM Type
mergeClassesT	 ts@(t:_)
 = do
 	let Just cids	= sequence 
			$ filter isJust
			$ map takeCidOfTClass ts

	let Just ks	= sequence $ map kindOfType ts

	case nub ks of
	 [k] ->	do	
		cid'	<- mergeClasses cids
		return	$ TClass k cid'
	
	 _ 	-> panic stage
		$ "mergeClassesT: " % ks % "\n"


-- | Clear the set of active classes.
clearActive ::	SquidM (Set ClassId)
clearActive
 = do	graph		<- gets stateGraph 
	
	active'		<- liftM Set.fromList
			$  mapM sinkClassId 
			$  Set.toList
			$  graphActive graph
	
	let graph'	= graph 
			{ graphActive = Set.empty }
	
	modify (\s -> s { stateGraph	= graph' })
			
	return		active'


-- | Activate a class, tagging it for inspection by the unifier \/ crusher.
--	Also activate any MPTCs acting on it.
activateClass :: ClassId -> SquidM ()
activateClass cid
 = do	-- traceM $ "activating class " % cid % "\n"

	graph		<- gets stateGraph
 	let graph'	= graph 
			{ graphActive	= Set.insert cid (graphActive graph) }

	modify (\s -> s { stateGraph = graph' })

	Just c		<- lookupClass cid
	(case c of
		Class { classFettersMulti = cidsMulti}
		 	-> mapM_ activateClass $ Set.toList cidsMulti
		_	-> return ())
		

-- Lookup the canconical name for this var.
sinkVar :: Var -> SquidM Var
sinkVar	var
 = do	mCid	<- lookupVarToClassId var
	let result
		| Nothing	<- mCid
		= return var

		| Just cid	<- mCid
		= do		
			var'		<- makeClassName cid 	
			return	$ var'
	
	result

-- rewrite all vars and cids in this thing to canconial form.
updateVC :: TransM SquidM a => a -> SquidM a
updateVC  z	
 = transZM 
 	transTableId
	 	{ transV	= sinkVar
		, transCid	= sinkClassId }
	z

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


-- | Walk down the left spine of this type to find the type in the bottom 
--	left node (if there is one)
--
--	For example, if the graph holds a type like:
--	   TApp (TApp (TCon tc) t1) t2
--	
--	Then starting from the cid of the outermost TApp, we'll walk down 
--	the left spine until we find (TCon tc), then return t1
--
--	If the node at the bottom of the spine hasn't been unified, then
--	It'll be a Nothing, so return that instead.
--
headTypeDownLeftSpine 
	:: ClassId 
	-> SquidM (Maybe Type)
	
headTypeDownLeftSpine cid1
 = do	Just cls1	<- lookupClass cid1

--	trace 	$ "    headTypeDownLeftSpine\n"
--		% "    cid1 = " % cid1			% "\n"
--		% "    type = " % classType cls1	% "\n\n"

	case classType cls1 of
	 Just (TApp (TClass _ cid11) t12)	
	   -> do Just cls11	<- lookupClass cid11
		 case classType cls11 of
			Just TCon{}	-> return $ Just t12
			_		-> headTypeDownLeftSpine cid11

	 _	-> return $ Nothing


-- Starting an outermost application node, trace the graph and return
--	a list of the parts being applied. Eg, tracing the following
--	structure from the graph gives [t1, t2, t3]
--
--         @
--       /   \
--      @     t3
--    /   \
--   t1    t2
--
-- If and of the nodes have Nothing for their type, then return Nothing.
--
traceDownLeftSpine
	:: Type
	-> SquidM (Maybe [Type])
	
traceDownLeftSpine tt
 = case tt of
	TClass _ cid
	 -> do	Just cls	<- lookupClass cid
		case classType cls of
			Just t@TBot{}	-> return $ Just [TClass (classKind cls) cid]
		 	Just t		-> traceDownLeftSpine t
			Nothing		-> return $ Nothing
			
	TApp t11 t12
	 -> do	mtsLeft	<- traceDownLeftSpine t11
		case mtsLeft of
			Just tsLeft	-> return $ Just (tsLeft ++ [t12])
			Nothing		-> return Nothing
			
	TCon{}
	 -> return $ Just [tt]
		
	_
	 -> panic stage
	 $  "traceDownLeftSpine: no match for " % tt

