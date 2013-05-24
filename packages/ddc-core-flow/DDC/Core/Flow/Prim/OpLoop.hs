
-- | Loop related names.
module DDC.Core.Flow.Prim.OpLoop
        ( readOpLoop
        , typeOpLoop
        , xLoopLoopN
        , xLoopGuard)
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

        OpLoopGuard     -> text "guard#"


-- | Read a loop operator name.
readOpLoop :: String -> Maybe OpLoop
readOpLoop str
 = case str of
        "loop#"         -> Just $ OpLoopLoop
        "loopn#"        -> Just $ OpLoopLoopN
        "guard#"        -> Just $ OpLoopGuard
        _               -> Nothing


-- Types ----------------------------------------------------------------------
-- | Yield the type of a loop operator.
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

        -- guard#  :: [k1 : Rate]. Ref# Nat# -> Bool# 
        --              -> ([k2 : Rate]. Nat# -> Unit) -> Unit
        OpLoopGuard 
         -> tForall kRate
         $  \_ -> tRef tNat
                `tFunPE` tBool
                `tFunPE` (tForall kRate $ \_ -> tNat `tFunPE` tUnit)
                `tFunPE` tUnit


-- Compounds ------------------------------------------------------------------
xLoopLoopN :: Type Name -> Exp () Name -> Exp () Name -> Exp () Name
xLoopLoopN tR xRN xF 
         = xApps () (xVarOpLoop OpLoopLoopN) [XType tR, xRN, xF]


xLoopGuard 
        :: Type Name    -- ^ Rate of outer context.
        -> Exp () Name  -- ^ Reference to guard counter.
        -> Exp () Name  -- ^ Boolean flag to test.
        -> Exp () Name  -- ^ Body of guard.
        -> Exp () Name

xLoopGuard tK1 xB xCount xF
        = xApps () (xVarOpLoop OpLoopGuard) [XType tK1, xCount, xB, xF]


-- Utils -----------------------------------------------------------------------
xVarOpLoop :: OpLoop -> Exp () Name
xVarOpLoop op
        = XVar () (UPrim (NameOpLoop op) (typeOpLoop op))
