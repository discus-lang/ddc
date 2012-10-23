
-- | Inline `ISet` meta-instructions.
--
--   It is helpful to be able to emit these during code generation,
--   but the LLVM compiler doesn't accept them directly.
module DDC.Llvm.Transform.Clean
        (Clean(..))
where
import DDC.Llvm.Module
import DDC.Llvm.Function
import DDC.Llvm.Instr
import Data.Map                 (Map)
import qualified Data.Map       as Map
import qualified Data.Foldable  as Seq
import qualified Data.Sequence  as Seq


class Clean a where
 clean     :: a -> a
 clean          = cleanWith Map.empty

 cleanWith :: Map Var Exp -> a -> a


instance Clean Module where
 cleanWith binds mm
  = mm  { modFuncs      
                = map (cleanWith binds)
                $ modFuncs mm }


instance Clean Function where
 cleanWith binds fun
  = fun { funBlocks     
                = cleanBlocks binds Map.empty [] 
                $ funBlocks fun }


-- | Clean set instructions in some blocks.
cleanBlocks 
        :: Map Var Exp          -- ^ Map of variables to their values.
        -> Map Var Label        -- ^ Map of variables to the label 
                                --    of the block they were defined in.
        -> [Block] 
        -> [Block] 
        -> [Block]

cleanBlocks _binds _defs acc []
        = reverse acc

cleanBlocks binds defs acc (Block label instrs : bs) 
 = let  (binds', defs', instrs2) 
                = cleanInstrs label binds defs [] 
                $ Seq.toList instrs

        instrs' = Seq.fromList instrs2
        block'  = Block label instrs'

   in   cleanBlocks binds' defs' (block' : acc) bs


-- | Clean set instructions in some instructions.
cleanInstrs 
        :: Label                -- ^ Label of the current block.
        -> Map Var Exp          -- ^ Map of variables to their values.
        -> Map Var Label        -- ^ Map of variables to the label
                                --    of the block they were defined in.
        -> [AnnotInstr] 
        -> [AnnotInstr] 
        -> (Map Var Exp, Map Var Label, [AnnotInstr])

cleanInstrs _blockLabel binds defs acc []
        = (binds, defs, reverse acc)

cleanInstrs blockLabel binds defs acc (ins@(AnnotInstr (i,annots)) : instrs)
  = let next binds' defs' acc' 
                = cleanInstrs blockLabel binds' defs' acc' instrs
        
        reAnnot i' = annotWith i' annots

        sub xx  
         = case xx of
                XVar v
                  | Just x' <- Map.lookup v binds
                  -> sub x'
                _ -> xx

    in case i of
        IComment{}              
         -> next binds defs (ins : acc)        

        -- The LLVM compiler doesn't support ISet instructions,                 -- TODO: do occ check
        --  so we inline them into their use sites.
        ISet v x                
         -> let binds'  = Map.insert v x binds
            in  next binds' defs acc

        -- The LLVM compiler doesn't support INop instructions,
        --  so we drop them out.         
        INop
         -> next binds defs acc

        -- At phi nodes, drop out joins of the 'undef' value.
        --  The converter adds these in rigtht before calling 'abort',
        --  so we can never arrive from one of those blocks.
        -- We also need to replace any 'unknown' labels with the
        --  label of the block each variable was defined in.
        IPhi v xls
         -> let 
                -- Don't merge undef expressions in phi nodes.
                keepPair (XUndef _)  = False
                keepPair _           = True

                -- Lookup the lable of the block a variable was defined in.
                getLabel x
                 |  XVar v'      <- x
                 ,  Just l       <- Map.lookup v' defs
                 = l

                 | otherwise    
                 = error $ unlines
                         [ "DDC.LLVM.Transform.Clean.cleanInstrs"
                         , "  Can't find join label for " ++ show x ]

                i'      = IPhi v [(sub x, getLabel x) 
                                        | (x, _) <- xls 
                                        , keepPair (sub x) ]

                defs'   = Map.insert v blockLabel defs
            in  next binds defs' $ (reAnnot i') : acc


        IReturn Nothing
         -> next binds defs $ ins                                       : acc

        IReturn (Just x)
         -> next binds defs $ (reAnnot $ IReturn (Just (sub x)))        : acc

        IBranch{}
         -> next binds defs $ ins                                       : acc

        IBranchIf x l1 l2
         -> next binds defs $ (reAnnot $ IBranchIf (sub x) l1 l2)       : acc

        ISwitch x def alts
         -> next binds defs $ (reAnnot $ ISwitch   (sub x) def alts)    : acc

        IUnreachable
         -> next binds defs $ ins                                       : acc

        IOp    v op x1 x2
         |  defs'        <- Map.insert v blockLabel defs
         -> next binds defs' $ (reAnnot $ IOp   v op (sub x1) (sub x2))  : acc

        IConv  v c x
         |  defs'        <- Map.insert v blockLabel defs
         -> next binds defs' $ (reAnnot $ IConv v c (sub x))             : acc

        ILoad  v x
         |  defs'        <- Map.insert v blockLabel defs
         -> next binds defs' $ (reAnnot $ ILoad v   (sub x))             : acc

        IStore x1 x2
         -> next binds defs  $ (reAnnot $ IStore    (sub x1) (sub x2))   : acc

        IICmp  v c x1 x2
         |  defs'        <- Map.insert v blockLabel defs
         -> next binds defs' $ (reAnnot $ IICmp v c (sub x1) (sub x2))   : acc

        IFCmp  v c x1 x2
         |  defs'        <- Map.insert v blockLabel defs
         -> next binds defs' $ (reAnnot $ IFCmp v c (sub x1) (sub x2))   : acc

        ICall  (Just v) ct t n xs ats
         |  defs'        <- Map.insert v blockLabel defs
         -> next binds defs' $ (reAnnot $ ICall (Just v) ct t n (map sub xs) ats) : acc

        ICall  Nothing ct t n xs ats
         -> next binds defs  $ (reAnnot $ ICall Nothing  ct t n (map sub xs) ats) : acc


