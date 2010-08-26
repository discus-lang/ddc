
-- | Wrappers for type graph operations. 
--   This module just makes them nicer to use in the `SquidM` monad, the real code is in "DDC.Solve.Graph".
module DDC.Solve.State.Graph
	( module DDC.Solve.Graph
	, lookupClass
	, kindOfClass
	, allocClass
	, ensureClassWithVar
	, modifyClass
	, updateClass
	, delSingleFetter
	, delMultiFetter
	, activateClass
	, clearActive
	, foldClasses
	, addNodeToClass)
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


-- | If there is already a class for this variable then return that
--   otherwise make a new one containing this var.
ensureClassWithVar :: TypeSource -> Kind -> Var	-> SquidM ClassId
ensureClassWithVar src kind var
 = do	mCid		<- lookupVarToClassId var
   	case mCid of
   	 Just cid	-> return cid
	 Nothing 
	  -> do	cid	<- allocClass kind src 
		addAliasForClass cid src var kind
	     	return cid


-- | Modify a class in the graph, and activate it.
modifyClass :: ClassId -> (Class -> Class) -> SquidM ()
modifyClass cid f
 = do	graph	<- getsRef stateGraph
	liftIO $ modifyClassInGraph cid graph f


-- | Update a class in the graph, and activate it.
updateClass :: ClassId -> Class -> SquidM ()
updateClass cid cls
 = do	graph	<- getsRef stateGraph
	liftIO $ modifyClassInGraph cid graph (\_ -> cls)


-- | Delete a SPTC Fetter from a class.
delSingleFetter :: ClassId -> Var -> SquidM ()
delSingleFetter cid v
 = 	modifyClass cid 
 	 $ \cls -> cls { classFetters = Map.delete v (classFetters cls) }
	

-- | Delete a fetter class from the type graph.
--
--   Note: This class *cannot* be re-used because it may have been deleted due to a MPTC
--	   being crushed out. Other nodes will still refer to this one, and Type.Trace 
--	   treats the ClassFetterDeleted as generating no constraints.
delMultiFetter :: ClassId -> SquidM ()
delMultiFetter cid
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


-- | Add a new node constraint to a class in the graph, and activate it.
addNodeToClass :: ClassId -> TypeSource -> Kind -> Node -> SquidM ()
addNodeToClass cid src kind node
 = do	graph	<- getsRef stateGraph
	graph'	<- liftIO $ addNodeToClassInGraph cid src kind node graph
	writesRef stateGraph graph'



				

