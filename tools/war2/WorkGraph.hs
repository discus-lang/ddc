-- | Work graphs
--		
module WorkGraph 
	( WorkGraph 
	, emptyWorkGraph
	, buildWorkGraphFromBackNodes
	, null
	, takeWork )
where

import BackGraph

import Prelude			hiding (null)
import Data.List 		hiding (null)

import qualified Data.Set 	as Set
import Data.Set			(Set)

import qualified Data.Map 	as Map
import Data.Map			(Map)

-- | A bidirectional work graph.
--	The nodes contain lists of parent jobs that have to be finished
--	before this one can be started, as well as the children that
--	are still waiting on this node.
--
data WorkGraph k
	= WorkGraph 
		(Set k)			-- root nodes that have no parents
		(Map k (WorkNode k))	-- the main graph
	deriving Show

data WorkNode k
	= WorkNode 
		[k] 			-- parents this node is waiting on
		[k]			-- children that are waiting for this node
	deriving (Eq, Show)


-- | An empty work graph with no nodes.
emptyWorkGraph :: WorkGraph k
emptyWorkGraph
	= WorkGraph Set.empty Map.empty


-- | Check if a work graph is empty
null :: WorkGraph k -> Bool
null (WorkGraph rootSet map)
	= Set.null rootSet && Map.null map
	

-- | Build a bidirectional work graph from a backwards one.
buildWorkGraphFromBackNodes
	:: Ord k
	=> [(k, BackNode k)] -> WorkGraph k

buildWorkGraphFromBackNodes nodes
 	= foldr addBackNode emptyWorkGraph nodes


-- | Add all the dependencies in backwards node to a work graph.
addBackNode
	:: Ord k
	=> (k, BackNode k) 
	-> WorkGraph k -> WorkGraph k

addBackNode
	(k, BackNode [])
	graph@(WorkGraph rootSet map)
	= WorkGraph (Set.insert k rootSet) map

addBackNode
 	(k, BackNode parents) 
	graph
	= foldr addDependency graph 
		$ zip parents (repeat k)


-- | Add a single dependency to a work graph.
addDependency
	:: Ord k
	=> (k, k) 
	-> WorkGraph k -> WorkGraph k

addDependency
	(kParent, kChild) 
	(WorkGraph rootSet map)

 	= WorkGraph (Set.delete kChild rootSet)
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


-- | Delete a parent from a second node.
--	Second line node = child of a root node.
deleteParentFromSecondNode
	:: Ord k
	=> k -> k
	-> WorkGraph k -> WorkGraph k

deleteParentFromSecondNode 
	kParent kChild 
	graph@(WorkGraph ksRoots map)
	| Just (WorkNode ksParents ksChildren)	<- Map.lookup kChild map
 	= let 	ksParents'	= delete kParent ksParents
		nChild'		= WorkNode ksParents' ksChildren
		ksRoots'	= case ksParents' of
					[]	-> Set.insert kChild ksRoots
					_	-> ksRoots
		map'		= Map.insert kChild nChild' map
	  in	WorkGraph ksRoots' map'


-- | Delete a dependency from the work graph
deleteRootNode
	:: Ord k
	=> k
	-> WorkGraph k -> WorkGraph k
	
deleteRootNode
	kParent
	graph@(WorkGraph rootSet map)
	
 | Set.member kParent rootSet 
 = let	rootSet'	= Set.delete kParent rootSet

	Just (WorkNode [] ksChildren)
			= Map.lookup kParent map

	map'		= Map.delete kParent map
	
	graph'	= foldr (\kChild g -> deleteParentFromSecondNode 
					kParent kChild g)
			(WorkGraph rootSet' map')
			ksChildren
   in	graph'		


-- | Take the ext piece of work from the graph
takeWork 
	:: Ord k
	=> WorkGraph k 
	-> (Maybe k, WorkGraph k)

takeWork graph@(WorkGraph rootSet map)
	| Set.null rootSet
	, Map.null map
	= (Nothing, graph)
	
takeWork graph@(WorkGraph rootSet map)
	| key : _	<- Set.toList rootSet
	= ( Just key
	  , deleteRootNode key graph)

