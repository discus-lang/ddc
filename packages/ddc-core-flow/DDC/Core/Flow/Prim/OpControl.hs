
-- | Control constructs used in lowered code.
module DDC.Core.Flow.Prim.OpControl
        ( readOpControl
        , typeOpControl
        , xLoopN
        , xGuard
        , xSegment
        , xSplit)
where
import DDC.Core.Flow.Prim.KiConFlow
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Flow.Prim.TyConFlow
import DDC.Core.Flow.Prim.Base
import DDC.Core.Exp.Simple.Compounds
import DDC.Core.Exp.Simple.Exp
import DDC.Base.Pretty
import Control.DeepSeq
import Data.Char
import Data.List


instance NFData OpControl where
 rnf !_ = ()


instance Pretty OpControl where
 ppr fo
  = case fo of
        OpControlLoop           -> text "loop#"
        OpControlLoopN          -> text "loopn#"
        OpControlGuard          -> text "guard#"
        OpControlSegment        -> text "segment#"
        OpControlSplit n        -> text "split$" <> int n <> text "#"


-- | Read a control operator name.
readOpControl :: String -> Maybe OpControl
readOpControl str
        | Just rest     <- stripPrefix "split$" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ OpControlSplit arity

        | otherwise
        = case str of
                "loop#"         -> Just $ OpControlLoop
                "loopn#"        -> Just $ OpControlLoopN
                "guard#"        -> Just $ OpControlGuard
                "segment#"      -> Just $ OpControlSegment
                _               -> Nothing


-- Types ----------------------------------------------------------------------
-- | Yield the type of a control operator.
typeOpControl  :: OpControl -> Type Name
typeOpControl op
 = case op of
        -- loop#   :: [k : Rate]. (Nat# -> Unit) -> Unit
        OpControlLoop
         -> tForall kRate 
         $  \_ -> (tNat `tFun` tUnit) `tFun` tUnit

        -- loopn#   :: [k : Rate]. RateNat# k -> (Nat# -> Unit) -> Unit
        OpControlLoopN
         -> tForall kRate 
         $  \kR -> tRateNat kR `tFun` (tNat `tFun` tUnit) `tFun` tUnit

        -- guard#   :: Ref# Nat# -> Bool# -> (Nat# -> Unit) -> Unit
        OpControlGuard 
         ->            tBool
                `tFun` (tUnit `tFun` tUnit)
                `tFun` tUnit

        -- segment# :: Ref Nat# -> Nat#  -> (Nat# -> Nat# -> Unit) -> Unit
        --   In the worker the first parameter is the index of the current
        --   element in the segment, and the second is the index into the 
        --   overall series.
        OpControlSegment
         ->            tNat
                `tFun` (tNat `tFun` tUnit)
                `tFun` tUnit

        -- split#  :: [k : Rate]. RateNat# k
        --         -> (RateNat# (Down8# k) -> Unit)
        --         -> (RateNat# (Tail8# k) -> Unit)
        --         -> Unit
        OpControlSplit n
         -> tForall kRate
          $ \tK -> tRateNat tK
                `tFun` (tRateNat (tDown n tK) `tFun` tUnit)
                `tFun` (tRateNat (tTail n tK) `tFun` tUnit)
                `tFun` tUnit


-- Compounds ------------------------------------------------------------------
type TypeF      = Type Name
type ExpF       = Exp () Name


xLoopN   :: TypeF -> ExpF -> ExpF -> ExpF
xLoopN tR xRN xF 
        = xApps (xVarOpControl OpControlLoopN) 
                [XType tR, xRN, xF]


xGuard   :: ExpF -> ExpF -> ExpF
xGuard xFlag xFun
        = xApps (xVarOpControl OpControlGuard) 
                [xFlag, xFun]


xSegment :: ExpF -> ExpF -> ExpF
xSegment xIters xFun
        = xApps (xVarOpControl OpControlSegment)
                [xIters, xFun]


xSplit  :: Int  -> TypeF -> ExpF
        -> ExpF -> ExpF  -> ExpF
xSplit n tK xRN xDownFn xTailFn 
        = xApps (xVarOpControl $ OpControlSplit n)
                [ XType tK, xRN, xDownFn, xTailFn ]


-- Utils -----------------------------------------------------------------------
xVarOpControl :: OpControl -> ExpF
xVarOpControl op
        = XVar (UPrim (NameOpControl op) (typeOpControl op))

