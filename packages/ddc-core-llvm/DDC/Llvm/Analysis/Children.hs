
module DDC.Llvm.Analysis.Children
        ( Children (..)
        , annotChildrenOfGraph
        , annotChildrenOfNode
        , childrenOfNode)
where
import DDC.Llvm.Syntax
import DDC.Llvm.Graph
import Data.Set                 (Set)
import qualified Data.Map       as Map


-- | The children of a node are the other nodes this one might branch to.
data Children
        = Children (Set Label)


-- | Annotate a graph with the children of each node.
annotChildrenOfGraph
        :: Graph a -> Graph (a, Children)

annotChildrenOfGraph (Graph entry nodes)
        = Graph entry
        $ Map.map annotChildrenOfNode nodes


-- | Annotate a node with its children.
annotChildrenOfNode 
        :: Node a -> Node (a, Children)

annotChildrenOfNode node@(Node label instrs annot)
 = Node label instrs
 $ (annot, Children $ childrenOfNode node)

