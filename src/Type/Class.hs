
module Type.Class
	( classChildren
	, expandGraph
	, allocClass
	, delClass
	, makeClassV
	, addToClass
	, lookupClass
	, updateClass
	, mergeClasses
	, mergeClassesT
	, sinkClassId
	, lookupVarToClassId
	, addBackRef
	, patchBackRef
	, makeClassName
	, clearActive
	, activateClass
	, addVarSubs
	, sinkVar

	, updateVC

	, kindOfCid
	, registerClass
	, unregisterClass)

where

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Data.Array.IO	as Array
import Data.Array.IO

import qualified Shared.Var	as Var
import qualified Shared.VarBind	as Var
import Shared.Var		(NameSpace(..))


import Util

import Shared.Error

import Type.Exp
import Type.Util
import Type.Plate.Trans
import Type.State
import Type.Plate.Collect

import qualified Type.School as School

-----
stage	= "Type.Squid.Class"

-- | Return the cids of all the children of this class.
classChildren :: Class -> [ClassId]
classChildren c
 = case c of
 	ClassFetter{}	-> collectClassIds $ classFetter c
	Class{}		-> collectClassIds $ classType c


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


-- | Delete a class by setting it to Nil and removing the backrefs from its children
delClass
	:: ClassId
	-> SquidM ()

delClass cid
 = do	Just c		<- lookupClass cid
	updateClass cid ClassNil

 	let cidChildren	= classChildren c
	mapM_ (delBackRef cid) cidChildren

	return ()	
	
	
-- | Delete a backref from the graph.
delBackRef
	:: ClassId 	-- parent node
	-> ClassId 	-- child node
	-> SquidM ()

delBackRef cidParent cidChild
 = do	
 	Just c	<- lookupClass cidChild
	let c'	= c { classBackRef = Set.delete cidParent (classBackRef c) }
	updateClass cidChild c'

	

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
		addToClass cid tSource (TVar kind v)

		----
		graph	<- gets stateGraph
		let graph'	= graph 
				{ graphVarToClassId = Map.insert v cid (graphVarToClassId graph) }

		modify (\s -> s { stateGraph = graph' })

	     	return	cid


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

	let c'		= addToClass2 cid src t c
	liftIO (writeArray (graphClass graph) cid c')
		
	activateClass cid
	return ()

addToClass2 cid src t c
 = case c of
 	ClassNil	-> addToClass3 src t (classInit cid (kindOfType t))
	Class{}		-> addToClass3 src t c
	
addToClass3 src t c
 = c   	{ classNodes	= (t, src) : classNodes c
  	, classType	= makeTUnify (classKind c) [classType c, t] }



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

	
-----
-- addClassForwards
--
addClassForwards ::	ClassId -> [ClassId] -> SquidM ()
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
	
	

-----
-- sinkClassId
--

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
		

		
-----
-- lookupVarToClassId
--
lookupVarToClassId :: 	Var -> SquidM (Maybe ClassId)
lookupVarToClassId v
 = do	graph		<- gets stateGraph
 	let vMap	= graphVarToClassId graph

	case Map.lookup v vMap of
	 Nothing	-> return Nothing
	 Just cid	
	  -> do	cid'	<- sinkClassId cid
	  	return	$ Just cid'
	 
	 
-----		
-- addBackRef
--
addBackRef	::  ClassId -> ClassId -> SquidM ()
addBackRef cid_ cidRef_ 
 = do 	cid	<- sinkClassId cid_
	cidRef	<- sinkClassId cidRef_
	
	Just c	<- lookupClass cid

	updateClass cid 
		c { classBackRef 
			= Set.insert cidRef (classBackRef c) }

-----
patchBackRef :: ClassId -> Maybe ClassId -> SquidM ()
patchBackRef	cidT mParent
 = case mParent of
 	Nothing		-> return ()
	Just cidP	-> addBackRef cidT cidP
	
	  
-----
-- unifyClasses
--	Doing this makes the class active.
mergeClasses
	:: (Kind -> [Type] -> Type)
	-> [ClassId] 
	-> SquidM ClassId

mergeClasses merge [cid_]
 = do	cid'		<- sinkClassId cid_
   	return	cid'
	
mergeClasses merge cids_
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
			, classType	= merge (classKind cL) (map classType cs)
			, classBackRef	= Set.unions $ map classBackRef cs }

	updateClass cidL cL'
	activateClass cidL

	-- Add forwards from old classes to new class.
	let cidsH	= cids \\ [cidL]
	addClassForwards cidL cidsH
	
  	return	$ cidL
		

mergeClassesT :: (Kind -> [Type] -> Type) -> [Type] -> SquidM Type
mergeClassesT	 merge ts@(t:_)
 = do
 	let Just cids	= sequence 
			$ map takeCidOfTClass ts
			
	cid'		<- mergeClasses merge cids
		
	return		$ TClass (kindOfType t) cid'


-- | Return a variable to identify this class.
--	If the class already contains variables then choose the one with the smallest display name
--	otherwise make a new variable and use that.
--	
makeClassName :: ClassId -> SquidM Var
makeClassName cid_
 = do	cid		<- sinkClassId cid_
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
		addToClass cid TSClassName (TVar kind v)
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
activateClass :: ClassId -> SquidM ()
activateClass cid
 = do	graph		<- gets stateGraph
 	
 	let graph'	= graph 
			{ graphActive	= Set.insert cid (graphActive graph) }
			
	modify (\s -> s { stateGraph = graph' })


-----
-- addVarSubs
--
addVarSubs ::	Var -> [Var]	-> SquidM ()
addVarSubs	varL_ 	vars_
 = do
 	varL	<- sinkVar varL_
	vars	<- mapM sinkVar vars_
	
 	sVarSub <##> \s 
		-> foldl (\m x -> Map.insert x varL m) s 
		$  filter (/= varL) vars
	
sinkVar ::	Var -> SquidM Var
sinkVar		var
 = do
 	sub	<- gets stateVarSub
	return	$ sinkVar' sub var
	
sinkVar' sub var
 = case Map.lookup var sub of
 	Nothing		-> var
	Just var'	-> sinkVar' sub var'
	


updateVC :: TransM SquidM a => a -> SquidM a
updateVC  z	
 = transZM 
 	transTableId
	 	{ transV	= sinkVar
		, transCid	= sinkClassId }
	z


kindOfCid :: ClassId -> SquidM Kind
kindOfCid cid
 = do	Just c	<- lookupClass cid
 	return	$ classKind c


-----
registerClass 
	:: Var.VarBind -> ClassId -> SquidM ()

registerClass bind cid
 = do	modify (\s -> s {
	 	stateRegister
			= Map.adjust 
				(Set.insert cid)
				bind
				(stateRegister s) })



unregisterClass
	:: Var.VarBind -> ClassId -> SquidM ()
	
unregisterClass bind cid
 = do 	register	<- gets stateRegister
	let Just reg	= Map.lookup bind register
	
	modify (\s -> s {
		stateRegister
			= Map.insert 
				bind
				(Set.delete cid reg)
				register })

	
