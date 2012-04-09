
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
  = fun { funBlocks     = map (cleanWith binds) $ funBlocks fun }


instance Clean Block where
 cleanWith binds (Block label instrs)
        = Block label 
        $ Seq.fromList 
        $ cleanInstrs binds
        $ Seq.toList instrs


cleanInstrs :: Map Var Exp -> [Instr] -> [Instr]
cleanInstrs _  [] = []

cleanInstrs bs (i : instrs)
  = let down    = cleanInstrs bs

        sub xx  
         = case xx of
                XVar v
                  | Just x' <- Map.lookup v bs
                  -> sub x'
                _ -> xx

    in case i of
        IComment{}              -> i                            : down instrs
        ISet v x                -> cleanInstrs (Map.insert v x bs) instrs                -- TODO: do occ check
        IReturn Nothing         -> i                            : down instrs
        IReturn (Just x)        -> IReturn (Just (sub x))       : down instrs
        IBranch{}               -> i                            : down instrs
        IBranchIf x l1 l2       -> IBranchIf (sub x) l1 l2      : down instrs
        ISwitch x def alts      -> ISwitch   (sub x) def alts   : down instrs
        IUnreachable            -> i                            : down instrs
        IOp    v op x1 x2       -> IOp   v op (sub x1) (sub x2) : down instrs
        IConv  v c x            -> IConv v c (sub x)            : down instrs
        ILoad  v x              -> ILoad v   (sub x)            : down instrs
        IStore x1 x2            -> IStore    (sub x1) (sub x2)  : down instrs
        IICmp  v c x1 x2        -> IICmp v c (sub x1) (sub x2)  : down instrs
        IFCmp  v c x1 x2        -> IFCmp v c (sub x1) (sub x2)  : down instrs
        ICall  mv ct t n xs ats -> ICall mv ct t n (map sub xs) ats : down instrs
