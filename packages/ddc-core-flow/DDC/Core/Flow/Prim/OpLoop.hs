
-- | Loop related names.
module DDC.Core.Flow.Prim.OpLoop
        ( readOpLoop
        , typeOpLoop)
where
import DDC.Core.Flow.Prim.KiConFlow
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Flow.Prim.TyConFlow
import DDC.Core.Flow.Prim.Base
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Base.Pretty
import Control.DeepSeq


instance NFData OpLoop


instance Pretty OpLoop where
 ppr fo
  = case fo of
        OpLoopLoop      -> text "loop#"
        OpLoopLoopN     -> text "loopn#"

-- | Read a baked-in loop operator.
readOpLoop :: String -> Maybe OpLoop
readOpLoop str
 = case str of
        "loop#"         -> Just $ OpLoopLoop
        "loopn#"        -> Just $ OpLoopLoopN

        _               -> Nothing


-- Types ----------------------------------------------------------------------
typeOpLoop  :: OpLoop -> Type Name
typeOpLoop op
 = case op of
        -- loop#  :: [k : Rate]. (Nat# -> Unit) -> Unit
        OpLoopLoop
         -> tForall kRate 
         $  \_ -> (tNat `tFunPE` tUnit) `tFunPE` tUnit

        -- loopn#  :: [k : Rate]. RateNat k -> (Nat# -> Unit) -> Unit
        OpLoopLoopN
         -> tForall kRate 
         $  \kR -> tRateNat kR `tFunPE` (tNat `tFunPE` tUnit) `tFunPE` tUnit
