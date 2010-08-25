
-- | Wrappers for type graph operations. 
--   This module just makes them nicer to use in the `SquidM` monad, the real code is in "DDC.Solve.Graph".
module DDC.Solve.State.Graph
	( module DDC.Solve.Graph
	, lookupClass
	, kindOfClass
	, allocClass
	, modifyClass
	, updateClass
	, delFetterClass
	, activateClass
	, clearActive
	, foldClasses
	
	, deleteSingleFetter
	, makeClassFromVar
	, addToClass
	, foldClasses)
where
import Type.Location
import DDC.Solve.State.Base
import DDC.Solve.State.Squid
import DDC.Solve.Graph
import DDC.Type
import DDC.Var
import Data.Array.IO
import Control.Monad.Trans
import Control.Monad
import Data.Set			(Set)
import qualified Data.Map	as Map
import {-# SOURCE #-} DDC.Solve.State.Naming


-- | Lookup a class from the graph.
lookupClass :: ClassId -> SquidM (Maybe Class)
lookupClass cid
 = do	graph	<- getsRef stateGraph
 	liftIO $ lookupClassFromGraph cid graph


-- | Get the kind of a class in the graph.
kindOfClass :: ClassId -> SquidM Kind
kindOfClass cid
 = do	Just c	<- lookupClass cid
 	return	$ classKind c


-- | Allocate a new class into the type graph.
allocClass :: Kind -> TypeSource -> SquidM ClassId
allocClass kind src
 = do	graph		<- getsRef stateGraph
	(cid, graph')	<- liftIO $ addClassToGraph (emptyClass kind src) graph
	writesRef stateGraph graph'
	return cid


-- | Modify a class in the graph using a given function.
modifyClass :: ClassId -> (Class -> Class) -> SquidM ()
modifyClass cid f
 = do	graph	<- getsRef stateGraph
	liftIO $ modifyClassInGraph cid graph f


-- | Update a class in the graph.
updateClass :: ClassId -> Class -> SquidM ()
updateClass cid cls
 = do	graph	<- getsRef stateGraph
	liftIO $ modifyClassInGraph cid graph (\_ -> cls)
	

-- | Delete a fetter class from the type graph.
--   Note: This class *cannot* be re-used because it may have been deleted due to a MPTC
--	   being crushed out. Other nodes will still refer to this one, and Type.Trace 
--	   treats the ClassFetterDeleted as generating no constraints.
delFetterClass :: ClassId -> SquidM ()
delFetterClass cid
 = do	graph	<- getsRef stateGraph
	liftIO $ delFetterFromGraph cid graph


-- | Activate a class, and any MPTC's acting on it.
activateClass :: ClassId -> SquidM ()
activateClass cid
 = do	graph	<- getsRef stateGraph
	liftIO 	$ activateClassOfGraph cid graph


-- | Clear the set of active classes.
clearActive ::	SquidM (Set ClassId)
clearActive
 = do	graph	<- getsRef stateGraph
	liftIO	$ clearActiveClassesOfGraph graph


-- | Fold a function through all the classes in the type graph.
foldClasses :: (a -> Class -> SquidM a) -> a -> SquidM a
foldClasses fun x
 = do  	graph		<- getsRef stateGraph
	classes		<- liftIO $ getElems $ graphClass graph
	foldM fun x classes  


-- TODO: push this stuff into DDC.Solve.Graph ----------------------------------------------------
-- | Delete a SPTC Fetter from a class.
deleteSingleFetter
	:: ClassId 
	-> Var
	-> SquidM ()
	
deleteSingleFetter cid v
 = do	Just cls	<- lookupClass cid
	let cls'	= cls { classFetters = Map.delete v (classFetters cls) }
	updateClass cid cls'


-- | If there is already a class for this variable then return that
--   otherwise make a new one containing this var.
makeClassFromVar
	:: TypeSource		-- ^ Source of the constraint containing the variable.
	-> Kind			-- ^ Kind of this variable.
	-> Var			-- ^ The variable.
	-> SquidM ClassId
		
makeClassFromVar src kind var
 = do	mCid		<- lookupVarToClassId var
   	case mCid of
   	 Just cid	-> return cid
	 Nothing 
	  -> do	cid	<- allocClass kind src 
		addAliasForClass cid src var kind
	     	return cid


-- | Add a new type constraint to a class
--	Doing this makes the class active.
addToClass 
	:: ClassId		-- ^ id of class to update
	-> TypeSource		-- ^ source of constraint
	-> Kind			-- ^ kind of the constraint
	-> Node			-- ^ constraint
	-> SquidM ()

addToClass cid src kind node
 = do	graph		<- getsRef stateGraph
	addToClass2 cid src kind node graph

addToClass2 cid' src kind node graph
 = go cid'
 where	go cid
	 = do	cls	<- liftIO (readArray (graphClass graph) cid)
		case cls of
		 ClassForward _ cid'' 	-> go cid''
		 ClassUnallocated	-> update' cid (emptyClass kind src cid)
		 Class{}		-> update' cid cls
		 	
	update' cid cls@Class{}
	 = do	liftIO 	$ writeArray (graphClass graph) cid 
			$ cls 	{ classUnified		= Nothing
				, classTypeSources	= (node, src) : classTypeSources cls }
			
		activateClass cid
		linkVar cid node
		
	linkVar cid tt
 	 = case tt of
 		NVar v
	 	 -> do	stateGraph `modifyRef` \graph' -> 
				graph' { graphVarToClassId = Map.insert v cid (graphVarToClassId graph) }

		_ -> return ()



				

