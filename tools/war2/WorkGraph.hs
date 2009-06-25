
-- | Work graphs
--	TODO: 	Don't invalidate the parent set when adding new work.
--		
module WorkGraph where

import BackGraph

import qualified Data.Set 	as Set
import Data.Set			(Set)

import qualified Data.Map 	as Map
import Data.Map			(Map)

-- A bidirectional work graph.
--	The nodes contain lists of parent jobs that have to be finished
--	before this one can be started, as well as the children that
--	are still waiting on this node.
--
data WorkGraph k
	= WorkGraph 
		(Maybe (Set k))		-- root nodes that have no parents
		(Map k (WorkNode k))	-- the main graph
	deriving Show

data WorkNode k
	= WorkNode 
		[k] 			-- parents this node is waiting on
		[k]			-- children that are waiting for this node
	deriving (Eq, Show)

emptyWorkGraph :: WorkGraph k
emptyWorkGraph
	= WorkGraph Nothing (Map.empty)


-- Build a bidirectional work graph from a backwards one
buildWorkFromBackGraph 
	:: Ord k
	=> BackGraph k -> WorkGraph k

buildWorkFromBackGraph (BackGraph map)
 	= foldr addBackNodeToWorkGraph emptyWorkGraph
		$ Map.toList map

-- Add all the dependencies in backwards node to a work graph
addBackNodeToWorkGraph 
	:: Ord k
	=> (k, BackNode k) 
	-> WorkGraph k -> WorkGraph k

addBackNodeToWorkGraph (k, BackNode parents) graph
	= foldr addDepToWorkGraph graph 
		$ zip parents (repeat k)


-- | Add a single dependency to a work graph
--	This just invalidates the current roots, so we'll need to load
--	them again the next time work is added.
--
addDepToWorkGraph 
	:: Ord k
	=> (k, k) 
	-> WorkGraph k -> WorkGraph k

addDepToWorkGraph 
	(kParent, kChild) 
	(WorkGraph _ map)

 	= WorkGraph Nothing
	$ Map.alter 
		(\nParent 
   		   -> case nParent of
			Nothing			-> Just (WorkNode [] [kChild])
			Just (WorkNode ps cs)	-> Just (WorkNode ps (cs ++ [kChild])))
		kParent
 
	$ Map.alter
		(\nChild
 		   -> case nChild of
			Nothing			-> Just (WorkNode [kParent] [])
			Just (WorkNode ps cs)	-> Just (WorkNode (ps ++ [kParent]) cs))
		kChild
	$ map


-- | Load the current root nodes.
--	These are the ones that can be worked on right away,
--	as they don't have parent nodes.


