{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Solve.Graph
	( Graph(..)
	, initialGraphSize
	, makeEmptyGraph)
where
import Data.Array.IO
import DDC.Type
import DDC.Solve.Class
import DDC.Var
import Data.Set			(Set)
import Data.Map			(Map)
import qualified Data.Set	as Set
import qualified Data.Map	as Map


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

