
module DDC.Llvm.Graph
        ( -- * Block Graphs
          Graph (..)
        , Node  (..)

          -- * Graph Utils
        , graphOfBlocks
        , blocksOfGraph
        , labelsOfGraph
        , lookupNodeOfGraph
        , modifyNodeOfGraph
        , mapNodesOfGraph
        , mapAnnotsOfGraph

          -- * Node Utils
        , blockOfNode
        , childrenOfNode)
where
import DDC.Llvm.Syntax
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


-- Graph Utils ----------------------------------------------------------------
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
 = go Set.empty [entry]
 where  
        -- The 'done' set records which nodes we've already visited. 
        -- We need this to handle join points, where there are multiple
        -- in-edges to the node.
        go _ []           = []
        go done (label : more)       
         = let  Just node = Map.lookup label nodes
                children  = childrenOfNode node

                -- Remember that we've already visited this node.
                done'     = Set.insert label done

                -- Add the children of this node to the set still to visit.
                more'     = Set.toList $ (Set.union (Set.fromList more) children)
                                         `Set.difference` done'

           in   Block label (nodeInstrs node) : go done' more'


-- | Get the set of all block labels in a graph.
labelsOfGraph :: Graph a -> [Label]
labelsOfGraph graph
        = map blockLabel $ blocksOfGraph graph


-- | Lookup a node from the graph, or `Nothing` if it can't be found.
lookupNodeOfGraph :: Graph a -> Label -> Maybe (Node a)
lookupNodeOfGraph (Graph _ nodes) label
        = Map.lookup label nodes


-- | Apply a function to a single node in the graoh.
modifyNodeOfGraph 
        :: Label                -- ^ Label of node to modify.
        -> (Node a -> Node a)   -- ^ Function to apply to the node.
        -> Graph a -> Graph a

modifyNodeOfGraph label modify graph@(Graph entry nodes)
 = case Map.lookup label nodes of
        Nothing         -> graph
        Just node       -> Graph entry (Map.insert label (modify node) nodes)


-- | Apply a function to every node in the graph.
mapNodesOfGraph :: (Node a -> Node b) -> Graph a -> Graph b
mapNodesOfGraph f (Graph entry nodes)
        = Graph entry $ Map.map f nodes


-- | Apply a function to every node annotation in the graph.
mapAnnotsOfGraph :: (a -> b) -> Graph a -> Graph b
mapAnnotsOfGraph f graph
 = let  modifyNode (Node label nodes annot) = Node label nodes (f annot)
   in   mapNodesOfGraph modifyNode graph


-- Node Utils -----------------------------------------------------------------
-- | Convert a `Node` to `Block` form, dropping any annotation.
blockOfNode :: Node a -> Block
blockOfNode (Node label instrs _)
        = Block label instrs


-- | Get the children of a node.
childrenOfNode :: Node a -> Set Label
childrenOfNode node
 = case Seq.viewr $ nodeInstrs node of
        Seq.EmptyR
                -> Set.empty

        _ Seq.:> instr    
                -> fromMaybe Set.empty
                $  branchTargetsOfInstr $ annotInstr instr

