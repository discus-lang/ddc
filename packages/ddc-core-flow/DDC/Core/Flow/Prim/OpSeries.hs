
module DDC.Core.Flow.Prim.OpSeries
        ( readOpSeries
        , typeOpSeries

        -- * Compounds
        , xRateOfSeries
        , xNatOfRateNat)
where
import DDC.Core.Flow.Prim.KiConFlow
import DDC.Core.Flow.Prim.TyConFlow
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Flow.Prim.Base
import DDC.Core.Compounds.Simple
import DDC.Core.Exp.Simple
import DDC.Base.Pretty
import Control.DeepSeq


instance NFData OpSeries


instance Pretty OpSeries where
 ppr pf
  = case pf of
        OpSeriesRateOfSeries      -> text "rateOfSeries"          <> text "#"
        OpSeriesNatOfRateNat      -> text "natOfRateNat"          <> text "#"


-- | Read a series operator name.
readOpSeries :: String -> Maybe OpSeries
readOpSeries str
 = case str of
        "rateOfSeries#" -> Just $ OpSeriesRateOfSeries
        "natOfRateNat#" -> Just $ OpSeriesNatOfRateNat
        _               -> Nothing


-- | Yield the type of a series operator.
typeOpSeries :: OpSeries -> Type Name
typeOpSeries op
 = case op of
        -- rateOfSeries#   :: [k : Rate]. [a : Data]
        --                 .  Series k a -> RateNat k
        OpSeriesRateOfSeries 
         -> tForalls [kRate, kData] $ \[tK, tA]
                -> tSeries tK tA `tFun` tRateNat tK

        -- natOfRateNat#   :: [k : Rate]. RateNat k -> Nat#
        OpSeriesNatOfRateNat 
         -> tForall kRate $ \tK 
                -> tRateNat tK `tFun` tNat


-- Compounds ------------------------------------------------------------------
xRateOfSeries :: Type Name -> Type Name -> Exp () Name -> Exp () Name
xRateOfSeries tK tA xS 
         = xApps  (xVarOpSeries OpSeriesRateOfSeries) 
                  [XType tK, XType tA, xS]


xNatOfRateNat :: Type Name -> Exp () Name -> Exp () Name
xNatOfRateNat tK xR
        = xApps  (xVarOpSeries OpSeriesNatOfRateNat)
                 [XType tK, xR]


-- Utils -----------------------------------------------------------------------
xVarOpSeries :: OpSeries -> Exp () Name
xVarOpSeries op
        = XVar  (UPrim (NameOpSeries op) (typeOpSeries op))
