{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Solve.State.Graph
	( Graph(..)
	, initialGraphSize
	, makeEmptyGraph
	, expandGraph
	, addClassToGraph)
where
import Data.Array.IO
import DDC.Type
import DDC.Solve.State.Class
import DDC.Var
import Data.Set			(Set)
import Data.Map			(Map)
import qualified Data.Set	as Set
import qualified Data.Map	as Map
import Control.Monad

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
	, graphActive		:: Set ClassId }	
					

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

 	return	Graph
		{ graphClass		= array
		, graphClassIdGen	= 0
		, graphVarToClassId	= Map.empty 
		, graphActive		= Set.empty }


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


-- | Allocate add new class to the type graph.
addClassToGraph :: (ClassId -> Class) -> Graph -> IO (ClassId, Graph)
addClassToGraph mkCls graph
 = do	graph'		<- expandGraph 1 graph

	let classIdGen	= graphClassIdGen graph'
 	let cid		= ClassId classIdGen

	writeArray (graphClass graph') cid (mkCls cid)

	return 	( cid
		, graph' { graphClassIdGen = classIdGen + 1 })

