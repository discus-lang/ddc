
module DDC.Llvm.Graph
        ( module DDC.Llvm.Exp
        , module DDC.Llvm.Type
        , module DDC.Llvm.Prim
        , module DDC.Llvm.Metadata

        -- | Block Graphs
        , Graph (..)
        , Node  (..)
        , graphOfBlocks
        , blocksOfGraph
        , labelsOfGraph
        , mapNodesOfGraph
        , modifyNodeOfGraph
        , childrenOfNode)
where
import DDC.Llvm.Exp
import DDC.Llvm.Type
import DDC.Llvm.Instr
import DDC.Llvm.Prim
import DDC.Llvm.Metadata
import Data.Maybe
import Data.Map                 (Map)
import Data.Set                 (Set)
import Data.Sequence            (Seq)
import qualified Data.Map       as Map
import qualified Data.Set       as Set
import qualified Data.Sequence  as Seq


-- | Llvm block graph.
--   We use this form for transformations, 
--   as it makes it easy to find blocks and attach annotations to them.
data Graph a
        = Graph 
        { -- | The entry node for the block graph.
          graphEntry    :: Label

          -- | Internal nodes.
        , graphNodes    :: Map Label (Node a) }
        deriving Show


-- | A block of instructions, and an optional annotation.
data Node a
        = Node
        { -- | Block label for the node.
          nodeLabel     :: Label

          -- | Statements in this node, with meta-data annotations.
        , nodeInstrs    :: Seq AnnotInstr

          -- | Optional annotation on the node.
        , nodeAnnot     :: a }
        deriving Show


-- | Convert a list of blocks to a block graph.
graphOfBlocks :: a -> [Block] -> Maybe (Graph a)
graphOfBlocks _ []      = Nothing
graphOfBlocks a blocks@(first : _)
 = let  entry   = blockLabel first
        nodes   = Map.fromList
                $ [ (label, Node label stmts a) 
                        | Block label stmts <- blocks ]
   in   Just $ Graph entry nodes


-- | Flatten a graph back into a list of blocks.
blocksOfGraph :: Graph a -> [Block]
blocksOfGraph (Graph entry nodes)
 = go [entry]
 where  go []                   = []
        go (label : more)       
         = let  Just node = Map.lookup label nodes
                children  = childrenOfNode node
                more'     = Set.toList $ Set.union (Set.fromList more) children
           in   Block label (nodeInstrs node) : go more'


-- | Get the set of all block labels in a graph.
labelsOfGraph :: Graph a -> [Label]
labelsOfGraph graph
        = map blockLabel $ blocksOfGraph graph


-- | Apply a function to every node in the graph.
mapNodesOfGraph :: (Node a -> Node b) -> Graph a -> Graph b
mapNodesOfGraph f (Graph entry nodes)
        = Graph entry $ Map.map f nodes


-- | Apply a function to a single node in the graoh.
modifyNodeOfGraph 
        :: Label                -- ^ Label of node to modify.
        -> (Node a -> Node a)   -- ^ Function to apply to the node.
        -> Graph a -> Graph a

modifyNodeOfGraph label modify graph@(Graph entry nodes)
 = case Map.lookup label nodes of
        Nothing         -> graph
        Just node       -> Graph entry (Map.insert label (modify node) nodes)


-- | Get the children of a node.
--   These are the blocks this node may transfer control to.
childrenOfNode :: Node a -> Set Label
childrenOfNode node
 = case Seq.viewr $ nodeInstrs node of
        Seq.EmptyR
                -> Set.empty

        _ Seq.:> instr    
                -> fromMaybe Set.empty
                $  branchTargetsOfInstr $ annotInstr instr


-- | Get a list of parents tracing back to the node that defines a given
--   variable, or Nothing if the definition site can not be found.
{- lineageOfVar
        :: Graph Parent 
        -> Label                -- Label of starting node.
        -> Var                  -- Variable we want the definition for.
        -> Maybe [Label]
-}
