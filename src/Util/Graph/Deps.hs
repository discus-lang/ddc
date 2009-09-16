
module Util.Graph.Deps
	( graphReachableS
	, graphReachable
	, graphReachable1_nr
	, graphSequence
	, graphSCC
	, graphPrune)
where

import qualified Debug.Trace	as Debug
import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)



graphReachableS
	:: Ord a
	=> Map a (Set a) -> Set a -> Set a
	
graphReachableS deps toVisit 
	= graphReachableS' deps toVisit Set.empty
	
graphReachableS' deps toVisit visited
 = case takeHead $ Set.toList toVisit of
 	Nothing	-> visited
	
	Just x
	 -> let	more		= fromMaybe Set.empty $ Map.lookup x deps
	 	visited'	= Set.insert x visited
		toVisit'	= (toVisit `Set.union` more) `Set.difference` visited'
	    in	graphReachableS' deps toVisit' visited'


-- elements reachable from this one, 
--	non-reflexative - so the starting element isn't considered to be reachable from itself.
--
graphReachable1_nr 
	:: Ord a
	=> Map a (Set a) -> a -> Set a
	
graphReachable1_nr deps start
 = let	Just toVisit	= Map.lookup start deps
   in	graphReachableS deps toVisit



-----
-- graphReachable
--	Works out all nodes in a graph reachable from a certain node.
--	-- BUGS: pretty sure this is wrong.
graphReachable 
	:: Ord a
	=> Map a [a] -> [a] -> Set a

graphReachable deps seed
	= graphReachable' deps Set.empty seed

graphReachable' deps visited []	= visited
graphReachable' deps visited (x:xs)
 	| Set.member x visited
 	= graphReachable' deps visited xs
	
	| otherwise
	= let	step	= fromMaybe [] (Map.lookup x deps)
	  in 	graphReachable' 
	  		deps
		  	(Set.union visited (Set.fromList step))
			(xs ++ step)
			

-- |	Do a depencency walk over a graph.
--
--	For each dependency xi -> yis, the yis are the elements which need to occur
--	before xi in the output list.
--
--	Nodes are marked as they are visited. For recursive groups of nodes, the output
--	order is the reverse of the order they are visited in.
--
--	Each node appears in the output list once and once only.
--	eg	For a list of dependencies
--			x0 -> [x1, x2, x3]
--			x1 -> [x1, x3]
--			x3 -> [x4, x5]
--
--		the output sequence is
--			[x4, x5, x3, x1, x2, x0]
--
graphSequence
	:: Ord a
	=> Map a [a] 	-- the graph.
	-> Set a	-- nodes already visited, don't include these in sequence.
	-> [a] 		-- nodes still to visited.
	-> [a]		-- sequence.

graphSequence graph visited []	= []
graphSequence graph visited (x:xs)
	| Set.member x visited
	= graphSequence graph visited xs
	
	| otherwise
	= let	xDeps		= fromMaybe [] (Map.lookup x graph)
		visited'	= Set.insert x visited
		more		= graphSequence graph visited' xDeps
	  in	more ++ [x] ++ graphSequence graph (Set.union (Set.fromList more) visited') xs
	  
	  


-- | Work out the strongly connected components in this graph starting from the given element.
--	
graphSCC :: Ord a => Map a (Set a) -> a	-> [Set a]
graphSCC graph x
	= graphSCC' graph Set.empty [] x

graphSCC' :: Ord a => Map a (Set a) -> Set a -> [a] -> a -> [Set a]
graphSCC' 
	graph 		-- the graph
	visited 	-- the set of nodes already visited
	path 		-- the path we've taken during the search (in reverse order)
	x		-- the current node
	
	-- If we've already visited this node then we've found a loop in the graph
 	| Set.member x visited
	= [Set.fromList path]
	
	-- Otherwise follow all the out edges
	| otherwise
	= let	out 		= fromMaybe Set.empty (Map.lookup x graph)
		visited'	= Set.insert x visited
	  in	concatMap (\o -> graphSCC' graph visited' (o : path) o) 
	  		  $ Set.toList out


-- | Prune a graph to just the elements reachable from this node
--
graphPrune :: Ord a => Map a (Set a) -> a -> Map a (Set a)
graphPrune graph k
 = case	Map.lookup k graph of
 	Just xs		-> Map.insert k xs (Map.unions $ map (graphPrune graph) $ Set.toList xs)
	Nothing		-> Map.empty
	


