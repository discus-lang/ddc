{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Solve.State.Graph
	( Graph(..)
	, initialGraphSize
	, makeEmptyGraph
	, expandGraph
	, lookupClassFromGraph
	, activateClassOfGraph
	, clearActiveClassesOfGraph
	, sinkClassIdOfGraph
	, addClassToGraph
	, delFetterFromGraph
	, modifyClassInGraph)
where
import Data.IORef
import Data.Array.IO
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Type
import DDC.Solve.State.Class
import DDC.Var
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
	  graphClass		:: IOArray ClassId Class		

	  -- | Generator for new ClassIds.
 	, graphClassIdGen	:: !Int					

	  -- | Map of type variables to the cids of the classes that contain them.
	, graphVarToClassId	:: Map Var ClassId

	-- | The classes which are active, meaning the ones that 
	--      have been touched recently and are waiting to be processed.
	, graphActive		:: IORef (Set ClassId) }	
					

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

	activeRef <- newIORef Set.empty

 	return	Graph
		{ graphClass		= array
		, graphClassIdGen	= 0
		, graphVarToClassId	= Map.empty 
		, graphActive		= activeRef }


-- | Increase the size of the type graph to contain at least a
--   certain number of free nodes.
expandGraph :: Int -> Graph -> IO Graph
expandGraph minFree graph
 = do	
	-- Get the current size
	ClassId curMax		<- liftM snd $ getBounds (graphClass graph)

	-- Get the first free class.
	let curIx		= graphClassIdGen graph
	
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

	 Class{}
	  -> do	modifyIORef (graphActive graph)
	  		$ Set.insert cid
	
		mapM_ (flip activateClassOfGraph graph)
			$ Set.toList 
			$ classFettersMulti cls
	
	 _ -> return ()	


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


-- | Allocate add new class to the type graph.
addClassToGraph :: (ClassId -> Class) -> Graph -> IO (ClassId, Graph)
addClassToGraph mkCls graph
 = do	graph'		<- expandGraph 1 graph

	let classIdGen	= graphClassIdGen graph'
 	let cid		= ClassId classIdGen

	-- write the new glass into the graph.
	writeArray  (graphClass graph') cid (mkCls cid)

	-- activate the class.
	activateClassOfGraph cid graph
	
	return 	( cid
		, graph' { graphClassIdGen = classIdGen + 1 })


-- | Modify a class in the graph.
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

