
module DDC.Core.Flow.Prim.KiConFlow
        ( readKiConFlow
        , kNatP
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
        KiConFlowNatP   -> text "Nat'"
        KiConFlowRate   -> text "Rate"


-- | Read a flow kind constructor.
readKiConFlow :: String -> Maybe KiConFlow
readKiConFlow str
 = case str of
        "Nat'"  -> Just $ KiConFlowNatP
        "Rate"  -> Just $ KiConFlowRate
        _       -> Nothing


-- Compounds ------------------------------------------------------------------
kNatP   = TCon (TyConBound (UPrim (NameKiConFlow KiConFlowNatP) sProp) sProp)
kRate   = TCon (TyConBound (UPrim (NameKiConFlow KiConFlowRate) sProp) sProp)
