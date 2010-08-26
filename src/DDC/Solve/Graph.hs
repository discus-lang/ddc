{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | The type graph and its primitive operations.
--
--   Most of the time the type inference will use the wrapped versions of these functions
--   that are located in "DDC.Solve.State.Graph".
module DDC.Solve.Graph
	( module DDC.Solve.Graph.Node
	, module DDC.Solve.Graph.Class
	, module DDC.Solve.Graph.Sink
	, Graph(..)
	, initialGraphSize
	, makeEmptyGraph
	, expandGraph
	, lookupClassFromGraph
	, activateClassOfGraph
	, clearActiveClassesOfGraph
	, sinkClassIdOfGraph
	, addClassToGraph
	, delFetterFromGraph
	, modifyClassInGraph
	, addNodeToClassInGraph
	, addAliasForClassInGraph)
where
import Type.Location
import DDC.Solve.Graph.Class
import DDC.Solve.Graph.Node
import DDC.Solve.Graph.Sink
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Type
import DDC.Var
import Data.IORef
import Data.Array.IO
import Data.Set			(Set)
import Data.Map			(Map)
import qualified Data.Set	as Set
import qualified Data.Map	as Map
import Control.Monad
import Data.Foldable		(foldlM)

stage	= "DDC.Solve.State.Graph"

-- | The type graph is an array of classes.
data Graph
	= Graph 
	{ -- | The classes.
	  graphClass		:: !(IOArray ClassId Class)

	  -- | Generator for new ClassIds.
 	, graphClassIdGen	:: !(IORef Int)

	  -- | Map of type variables to the cids of the classes that contain them.
	, graphVarToClassId	:: !(Map Var ClassId)

	-- | The classes which are active, meaning the ones that 
	--      have been touched recently and are waiting to be processed.
	, graphActive		:: !(IORef (Set ClassId)) }	
					

{-# INLINE modifyArray #-}
modifyArray :: Ix ix => IOArray ix c -> ix -> (c -> c) -> IO ()
modifyArray arr i f
 = do	x	<- readArray arr i 
	writeArray arr i (f x)


-- | Initial size of the graph.
initialGraphSize :: Int
initialGraphSize = 1000

-- | Make a new empty graph.
makeEmptyGraph :: IO Graph
makeEmptyGraph
 = do	-- All the classes start out unallocaed.
	array	<- newArray 
			(ClassId 0, ClassId initialGraphSize) 
			ClassUnallocated

	activeRef	<- newIORef Set.empty
	cidGenRef	<- newIORef 0

 	return	Graph
		{ graphClass		= array
		, graphClassIdGen	= cidGenRef
		, graphVarToClassId	= Map.empty 
		, graphActive		= activeRef }


-- | Increase the size of the type graph to contain at least a
--   certain number of free nodes.
expandGraph :: Int -> Graph -> IO Graph
expandGraph minFree graph
 = do	-- Get the current size.
	ClassId curMax		<- liftM snd $ getBounds (graphClass graph)

	-- Get the first free class.
	curIx			<- readIORef $ graphClassIdGen graph
	
	-- Check if there's enough room, 
	--	if not then make a new one twice the size and 
	--	copy in all the old classes.
	if curIx + minFree <= curMax
	 then return graph
	 else do 
	 	let newMax	= curMax * 2

		elems	 	<- getElems (graphClass graph)
		newClass	<- newListArray (ClassId 0, ClassId newMax)
				$  elems ++ replicate curMax ClassUnallocated

		return graph	{ graphClass = newClass }


-- | Lookup a class from the graph.
lookupClassFromGraph :: ClassId -> Graph -> IO (Maybe Class)
lookupClassFromGraph cid graph
 = do	cls	<- readArray (graphClass graph) cid 
	case cls of
	 ClassForward _ cid'	-> lookupClassFromGraph cid' graph
	 ClassUnallocated{}	-> return Nothing
	 _			-> return $ Just cls


-- | Activate a class, and any MPTCs acting on it.
activateClassOfGraph :: ClassId -> Graph -> IO ()
activateClassOfGraph cid graph
 = do	cls	<- readArray (graphClass graph) cid
	case cls of
	 ClassForward _ cid'	
	  -> activateClassOfGraph cid' graph

	 -- TODO: something is trying to activate unallocated classes. 
	 --       Is this happening when we alloc a fresh one?
	 --       Hunt it down and stop it from doing this.
	 ClassUnallocated{}
	  -> return ()
	
	 Class{}
	  -> do	modifyIORef (graphActive graph)
	  		$ Set.insert cid
	
		mapM_ (flip activateClassOfGraph graph)
			$ Set.toList 
			$ classFettersMulti cls
	
	 ClassFetter{}
	  -> 	modifyIORef (graphActive graph)
			$ Set.insert cid
	
	 -- If we activate a node that was constrained by some previously 
	 -- deleted fetter this also tries to activate the deleted version.
	 -- This is ok, we just ignore the attempted activation.
	 ClassFetterDeleted{}
	  -> return ()
	

-- | Get the set of active classes from the graph, emptying the contained set.
clearActiveClassesOfGraph :: Graph -> IO (Set ClassId)
clearActiveClassesOfGraph graph
 = do	active	<- readIORef (graphActive graph)
	writeIORef (graphActive graph) Set.empty

	-- Sink all the cids in the active set.
	foldlM 	(\set cid
		  -> do	cid'	<- sinkClassIdOfGraph cid graph
			return	$ Set.insert cid' set)
		Set.empty
		active


-- | Get the canonical version of this classid.
sinkClassIdOfGraph :: ClassId -> Graph -> IO ClassId
sinkClassIdOfGraph cid graph
 = do	cls	<- readArray (graphClass graph) cid
	case cls of
	 ClassForward _ cid'	-> sinkClassIdOfGraph cid' graph
	 _			-> return cid


-- | Allocate add new class to the type graph, and activate it.
addClassToGraph :: (ClassId -> Class) -> Graph -> IO (ClassId, Graph)
addClassToGraph mkCls graph
 = do	graph'		<- expandGraph 1 graph
	classIdGen	<- readIORef $ graphClassIdGen graph'
 	let cid		= ClassId classIdGen

	-- write the new glass into the graph.
	writeArray  (graphClass graph') cid (mkCls cid)

	-- update the cid generator
	writeIORef  (graphClassIdGen graph') (classIdGen + 1)
	
	-- activate the class.
	activateClassOfGraph cid graph
	
	return (cid, graph')


-- | Modify a class in the graph, and activate it.
modifyClassInGraph :: ClassId -> Graph -> (Class -> Class) -> IO ()
modifyClassInGraph cid graph f
 = do	cls	<- readArray (graphClass graph) cid
	case cls of
	 ClassForward _ cid'
	  -> 	modifyClassInGraph cid' graph f

	 _ -> do
		writeArray (graphClass graph) cid (f cls)
		activateClassOfGraph cid graph
		

-- | Delete a fetter class in the graph.
--   If the specified class is not a `ClassFetter` then `panic`.
delFetterFromGraph :: ClassId -> Graph -> IO ()
delFetterFromGraph cid graph
 = modifyArray (graphClass graph) cid 
 $ \cls -> 
	case cls of
	 ClassFetter{}	-> ClassFetterDeleted cls
	 _		-> panic stage 
				$ "delFetterFromGraph: class " % cid
				% " to be deleted is not a ClassFetter{}"


-- | Add a new type constraint to a class, and activate it.
--	If there is already a class with this cid then we add the node to it,
--	otherwise we allocate a new class.
addNodeToClassInGraph :: ClassId -> Kind -> TypeSource -> Node -> Graph -> IO Graph
addNodeToClassInGraph cid kind src node graph
 = follow cid
 where
	follow cid' 
	 = do	cls	<- readArray (graphClass graph) cid'
		case cls of
	 	 ClassForward _ cid''	-> follow cid''
	 	 ClassUnallocated	-> update cid' (emptyClass kind src cid')
 	 	 Class{}		-> update cid' cls
	 	 _			-> death
			 	
	update cid' cls
	 = case cls of
	    Class{} -> do
	 	writeArray (graphClass graph) cid'
			$ cls 	{ classUnified		= makeUnified cls
				, classTypeSources	= (node, src) : classTypeSources cls }
			
		activateClassOfGraph cid' graph

		(case node of
			NVar v	-> return $ graph { graphVarToClassId = Map.insert v cid (graphVarToClassId graph) }
			_	-> return $ graph)
			
	    _ -> death

	-- if there is nothing already in this class we can set the classUnified.
	makeUnified cls
	 = case cls of
	    Class{}
		 | isRegionKind kind 		-> Nothing
		 | isEffectKind kind 		-> Nothing
		 | isClosureKind kind		-> Nothing
		 | null $ classTypeSources cls	-> Just node
		 | otherwise	 		-> Nothing
		
	    _ -> death
	
	death 	= panic stage $ "addNodeToClassInGraph: wrong kind of class"


-- | Add an alias for a class.
--   An alias is a name that identifies the class. There can be many aliases 
--   for a given class, but only one ''canonical'' name.
addAliasForClassInGraph :: ClassId -> Kind -> TypeSource -> Var	-> Graph -> IO Graph
addAliasForClassInGraph cid kind src var graph
 = do	modifyClassInGraph cid graph
 	 $ \cls -> case cls of
		ClassUnallocated{}
		 -> (emptyClass kind src cid) 
			{ className	= Just var
			, classAliases 	= Map.singleton var src }
			
		Class { className = Nothing }
		 -> cls	{ className	= Just var
			, classAliases 	= Map.insert var src (classAliases cls) } 

		Class { className = Just _ }
		 -> cls	{ classAliases 	= Map.insert var src (classAliases cls) } 

		_ -> panic stage 
			$ "addAliasForClass: can't modify class " % cid
	
	return 	$ graph
		{ graphVarToClassId = Map.insert var cid (graphVarToClassId graph) }

	
