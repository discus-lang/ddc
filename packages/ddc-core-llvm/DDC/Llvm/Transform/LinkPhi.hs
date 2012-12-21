
module DDC.Llvm.Transform.LinkPhi
        (linkPhi)
where
import DDC.Llvm.Module
import DDC.Llvm.Graph
import DDC.Llvm.Function
import DDC.Llvm.Instr
import DDC.Llvm.Analysis.Parents
import qualified Data.Sequence  as Seq


-- | Link Phi instructions in a module.
--
--   For Phi instructions, the Salt->Llvm converter just fills in the source
--   block label of each variable to be merged with 'undef'. We need to add
--   the real block label of the in-edge that defines each variable.
--
--   We build a graph of each block, work out the in-edges due to branches,
--   and fill in the real block labels by back tracing the in-edges until we
--   find the node that defines each variable.
--
linkPhi :: Module -> Module
linkPhi mm
 = mm { modFuncs = map (linkPhiFunction) $ modFuncs mm }


-- | Link Phi instructions in a function.
linkPhiFunction :: Function -> Function
linkPhiFunction fun
 = fun  { funBlocks 
                = let Just graph = graphOfBlocks () (funBlocks fun) 
                  in  blocksOfGraph
                        $ linkPhiGraph graph }


-- | Link Phi instructions in a block graph.
linkPhiGraph :: Graph () -> Graph Parents
linkPhiGraph graph
 = let  graph'  = mapAnnotsOfGraph snd 
                $ annotParentsOfGraph graph
   in   mapNodesOfGraph (linkPhiNode graph') graph'


-- | Link Phi instructions in a node.
linkPhiNode :: Graph Parents -> Node Parents -> Node Parents
linkPhiNode graph node@(Node label instrs parents)
 | (Seq.viewl -> instr Seq.:< rest)       <- instrs
 = case instr of
        -- If a block has a Phi instruction then it always comes first.
        AnnotInstr IPhi{} _
         -> let Just instr'  = linkPhiInstr graph label instr
            in  Node label (instr' Seq.<| rest) parents

        _ -> node

 | otherwise
 = node


-- | Link the block labels in this Phi instruction.
linkPhiInstr 
        :: Graph Parents  -- ^ Block graph of the whole function body.
        -> Label          -- ^ Label of the block this instruction is in.
        -> AnnotInstr     -- ^ The Phi instruction to link.
        -> Maybe AnnotInstr

linkPhiInstr graph lNode (AnnotInstr (IPhi vDst xls) meta)
 = Just $ AnnotInstr (IPhi vDst xls') meta
 where  
        -- Link all the labels in the Phi instruction.
        xls'    = [(x, linkLabel x lMerge) | (x, lMerge) <- xls]

        -- Find the in-edge that defines this variable.
        --  We use 'lineageOfVar' to get the list of in-edges all the
        --  way back to the use-site. The parent node of the current one
        --  is then second in the list.
        linkLabel (XVar var) lMerge
         = case lineageOfVar graph var lNode of
                Just (_ : lParent : _)  -> lParent
                _                       -> lMerge

        -- If we can't find the definition then just return the
        -- original label.
        linkLabel _ lMerge              =  lMerge


linkPhiInstr _graph _ _
        = Nothing

