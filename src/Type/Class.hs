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

	, kindOfCid)

where

import Type.Exp
import Type.Location
import Type.Util.Bits
import Type.Plate.Trans
import Type.State
import Type.Plate.Collect

import Util

import qualified Shared.Var	as Var
import qualified Shared.VarBind	as Var
import qualified Shared.VarUtil	as Var
import Shared.Var		(NameSpace(..))
import Shared.Error

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Data.Array.IO	as Array
import Data.Array.IO

-----
-- stage	= "Type.Squid.Class"

-- | Return the cids of all the children of this class.
classChildren :: Class -> [ClassId]
classChildren c
 = case c of
 	ClassFetter { classFetter = f }	-> collectClassIds f
	Class	    { classType   = mt } 
	 -> fromMaybe [] $ liftM collectClassIds mt


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
	:: Kind			-- The type of the graph.
	-> SquidM ClassId

allocClass kind
 = do	expandGraph	1

	graph		<- gets stateGraph
	let classIdGen	=  graphClassIdGen graph
 	let cid		= ClassId classIdGen

	liftIO (writeArray (graphClass graph) cid (classInit cid kind))

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
	  -> do	cid	<- allocClass kind
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

	let vars	= catMaybes
			$ map (\t -> case t of
					TVar k v		-> Just v
					_			-> Nothing)
			$ map fst
			$ classNodes c
			
	case vars of
	 [] 
	  -> do	v	<- newVarN (spaceOfKind kind)
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
 = case c of
 	ClassNil	-> addToClass3 cid src t (classInit cid (kindOfType t))
	Class{}		-> addToClass3 cid src t c
	
addToClass3 cid src t c@Class{}
 = do 	activateClass cid
 	return	$ c	{ classNodes	= (t, src) : classNodes c
			, classType	= Nothing
		  	, classQueue	= (maybeToList $ classType c) ++ classQueue c ++ [t] }

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
 	ClassNil	-> addNameToClass3 cid src t (classInit cid (kindOfType t))
	Class{}		-> addNameToClass3 cid src t c
	
addNameToClass3 cid src t c
 = do 	activateClass cid
 	return	$ c	{ classNodes	= (t, src) : classNodes c }


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
	 
	 
  -- | Merge two classes by concatenating their queue and node list
--	The one with the lowesed classId gets all the constraints and the others 
--	are updated to be ClassFowards which point to it.
mergeClasses
	:: [ClassId] 
	-> SquidM ClassId

mergeClasses [cid_]
 = do	cid'		<- sinkClassId cid_
   	return	cid'
	
mergeClasses cids_
 = do	
	-- Sink the cids and lookup their classes.
 	cids		<- liftM nub $ mapM (\cid -> sinkClassId cid) cids_

	cs		<- liftM (map (\(Just c) -> c))
			$  mapM  (\cid -> lookupClass cid)
			$  cids
	
	-- The class with the lowest cid gets all the items.
	let Just cidL	= takeMinimum cids
	Just cL		<- lookupClass cidL

	let cL'		= cL 	
			{ classNodes	= concat $ map classNodes cs
			, classType	= Nothing

			, classQueue	=  nub
					$  catMaybes
					$  map (\t -> case t of
							TBot _	-> Nothing
							_	-> Just t)
					$  concat (map classQueue cs)
					++ concat (map (maybeToList . classType) cs)

			, classFetters	
				= concat $ map classFetters cs

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
			$ map takeCidOfTClass ts

	cid'		<- mergeClasses cids
		
	return		$ TClass (kindOfType t) cid'


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
 = do	graph		<- gets stateGraph
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



