
module DDC.Llvm.Analysis.Parents
        ( Parents (..)
        , annotParentsOfGraph
        , lineageOfVar)
where
import DDC.Llvm.Graph
import DDC.Llvm.Syntax
import Data.Set                 (Set)
import qualified Data.Set       as Set
import qualified Data.Map       as Map
import Data.Maybe


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


-- | Get a list of parents tracing back to the node that defines the given
--   variable, or `Nothing` if the definition site can not be found.
lineageOfVar
        :: Graph Parents
        -> Var                  -- Variable we want the definition for.
        -> Label                -- Label of starting node.
        -> Maybe [Label]

lineageOfVar graph target start
 = go start
 where  go label
         | Just node    <- lookupNodeOfGraph graph label
         , defs         <- defVarsOfBlock $ blockOfNode node
         = if Set.member target defs 
            -- We found the defining node.
            then Just [nodeLabel node]

            -- We haven't found the definining node yet, 
            -- so check the parents.
            else let Parents parents = nodeAnnot node
                     psLines    = map (lineageOfVar graph target)
                                $ Set.toList parents
                 in  case catMaybes psLines of
                        line : _        -> Just (nodeLabel node : line)
                        _               -> Nothing

         | otherwise
         = Nothing
