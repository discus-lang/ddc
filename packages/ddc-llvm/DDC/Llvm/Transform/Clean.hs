
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
 clean  = cleanWith Map.empty

 cleanWith :: Map Var Exp -> a -> a


instance Clean Module where
 cleanWith binds mm
  = mm  { modFuncs      = map (cleanWith binds) $ modFuncs mm }


instance Clean Function where
 cleanWith binds fun
  = fun { funBlocks     = cleanBlocks binds [] $ funBlocks fun }


-- | Clean set instructions in some blocks.
cleanBlocks 
        :: Map Var Exp 
        -> [Block] -> [Block] -> [Block]

cleanBlocks _binds acc []
        = reverse acc

cleanBlocks binds acc (Block label instrs : bs) 
 = let  (binds', instrs2) 
                = cleanInstrs binds [] 
                $ Seq.toList instrs

        instrs' = Seq.fromList instrs2
        block'  = Block label instrs'

   in   cleanBlocks binds' (block' : acc) bs


-- | Clean set instructions in some instructions.
cleanInstrs 
        :: Map Var Exp 
        -> [Instr] -> [Instr] -> (Map Var Exp, [Instr])

cleanInstrs binds acc []
        = (binds, reverse acc)

cleanInstrs binds acc (i : instrs)
  = let next binds' acc' 
                = cleanInstrs binds' acc' instrs

        sub xx  
         = case xx of
                XVar v
                  | Just x' <- Map.lookup v binds
                  -> sub x'
                _ -> xx

    in case i of
        IComment{}              -> next binds (i                                : acc)        
        ISet v x                -> next (Map.insert v x binds)                    acc        -- TODO: do occ check
        IReturn Nothing         -> next binds (i                                : acc)
        IReturn (Just x)        -> next binds (IReturn (Just (sub x))           : acc)
        IBranch{}               -> next binds (i                                : acc)
        IBranchIf x l1 l2       -> next binds (IBranchIf (sub x) l1 l2          : acc)
        ISwitch x def alts      -> next binds (ISwitch   (sub x) def alts       : acc)
        IUnreachable            -> next binds (i                                : acc)
        IOp    v op x1 x2       -> next binds (IOp   v op (sub x1) (sub x2)     : acc)
        IConv  v c x            -> next binds (IConv v c (sub x)                : acc)
        ILoad  v x              -> next binds (ILoad v   (sub x)                : acc)
        IStore x1 x2            -> next binds (IStore    (sub x1) (sub x2)      : acc)
        IICmp  v c x1 x2        -> next binds (IICmp v c (sub x1) (sub x2)      : acc)
        IFCmp  v c x1 x2        -> next binds (IFCmp v c (sub x1) (sub x2)      : acc)
        ICall  mv ct t n xs ats -> next binds (ICall mv ct t n (map sub xs) ats : acc)


