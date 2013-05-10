
module DDC.Core.Flow.Prim.KiConFlow
        ( readKiConFlow
        , kRate)
where
import DDC.Core.Flow.Prim.Base
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Base.Pretty
import Control.DeepSeq


instance NFData KiConFlow


instance Pretty KiConFlow where
 ppr con
  = case con of
        KiConFlowRate   -> text "Rate"


-- | Read a kind constructor name.
readKiConFlow :: String -> Maybe KiConFlow
readKiConFlow str
 = case str of
        "Rate"  -> Just $ KiConFlowRate
        _       -> Nothing


-- Compounds ------------------------------------------------------------------
kRate   = TCon (TyConBound (UPrim (NameKiConFlow KiConFlowRate) sProp) sProp)
