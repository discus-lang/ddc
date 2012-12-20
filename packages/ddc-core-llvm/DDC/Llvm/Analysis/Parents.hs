
module DDC.Llvm.Analysis.Parents
        ( Parents (..)
        , annotParentsOfGraph)
where
import DDC.Llvm.Instr
import DDC.Llvm.Graph
import Data.Set                 (Set)
import qualified Data.Set       as Set
import qualified Data.Map       as Map


-- | The parents of a node are the other nodes that might branch
--   to this one.
data Parents
        = Parents (Set Label)


-- | Annotate a graph with the parents of each node.
annotParentsOfGraph
        :: Graph a -> Graph (a, Parents)

annotParentsOfGraph graph0
 = go (zeroParents graph0)
 $ labelsOfGraph graph0
 where  
        go graph []
         = graph

        go graph (label : rest)
         = go (pushParents label graph) rest

        -- Add this node as a parent of its children.
        pushParents label graph
         = let  Just node       = Map.lookup label $ graphNodes graph
                lsChildren      = childrenOfNode node
           in   foldr (addParent label) graph $ Set.toList lsChildren

        -- Add a parent to a child node
        addParent labelParent labelChild graph
         = flip (modifyNodeOfGraph labelChild) graph $ \node 
         -> let (a, Parents ls) = nodeAnnot node
                annot'          = (a, Parents (Set.insert labelParent ls))
            in  node { nodeAnnot = annot' }

        -- Add empty parent sets to all the nodes in a graph.
        zeroParents graph
         = flip mapNodesOfGraph graph
         $ \node  -> node { nodeAnnot = (nodeAnnot node, Parents Set.empty) }
