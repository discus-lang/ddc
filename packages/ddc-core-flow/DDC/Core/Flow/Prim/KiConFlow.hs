
module DDC.Core.Flow.Prim.KiConFlow
        ( readKiConFlow
        , kRate
        , kProc )
where
import DDC.Core.Flow.Prim.Base
import DDC.Core.Compounds
import DDC.Core.Exp.Simple
import DDC.Base.Pretty
import Control.DeepSeq


instance NFData KiConFlow


instance Pretty KiConFlow where
 ppr con
  = case con of
        KiConFlowRate   -> text "Rate"
        KiConFlowProc   -> text "Proc"


-- | Read a kind constructor name.
readKiConFlow :: String -> Maybe KiConFlow
readKiConFlow str
 = case str of
        "Rate"  -> Just $ KiConFlowRate
        "Proc"  -> Just $ KiConFlowProc
        _       -> Nothing


-- Compounds ------------------------------------------------------------------
kRate   = TCon (TyConBound (UPrim (NameKiConFlow KiConFlowRate) sProp) sProp)

kProc   = TCon (TyConBound (UPrim (NameKiConFlow KiConFlowProc) sProp) sProp)
