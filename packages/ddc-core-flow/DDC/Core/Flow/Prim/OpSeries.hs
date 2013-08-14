
module DDC.Core.Flow.Prim.OpSeries
        ( readOpSeries
        , typeOpSeries

        -- * Compounds
        , xRateOfSeries
        , xNatOfRateNat
        , xNext
        , xNextC)
where
import DDC.Core.Flow.Prim.KiConFlow
import DDC.Core.Flow.Prim.TyConFlow
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Flow.Prim.Base
import DDC.Core.Compounds.Simple
import DDC.Core.Exp.Simple
import DDC.Base.Pretty
import Control.DeepSeq
import Data.List
import Data.Char


instance NFData OpSeries


instance Pretty OpSeries where
 ppr pf
  = case pf of
        OpSeriesRateOfSeries    -> text "rateOfSeries"  <> text "#"
        OpSeriesNatOfRateNat    -> text "natOfRateNat"  <> text "#"

        OpSeriesNext 1          -> text "next#"
        OpSeriesNext n          -> text "next$"         <> int n <> text "#"

        OpSeriesDown n          -> text "down$"         <> int n <> text "#"
        OpSeriesTail n          -> text "tail$"         <> int n <> text "#"


-- | Read a series operator name.
readOpSeries :: String -> Maybe OpSeries
readOpSeries str
        | Just rest     <- stripPrefix "next$" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , n >= 1
        = Just $ OpSeriesNext n

        | Just rest     <- stripPrefix "down$" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , n >= 1
        = Just $ OpSeriesDown n

        | Just rest     <- stripPrefix "tail$" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , n >= 1
        = Just $ OpSeriesTail n

        | otherwise
        = case str of
                "rateOfSeries#" -> Just $ OpSeriesRateOfSeries
                "natOfRateNat#" -> Just $ OpSeriesNatOfRateNat
                "next#"         -> Just $ OpSeriesNext 1
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

        -- next#   :: [a : Data]. [k : Rate]. Series# k a -> Nat# -> a
        OpSeriesNext 1
         -> tForalls [kData, kRate]
         $  \[tA, tK] -> tSeries tK tA `tFun` tNat `tFun` tA

        -- next$N# :: [a : Data]. [k : Rate]
        --         .  Series# (DownN# k) a -> Nat# -> VecN# a
        OpSeriesNext n
         -> tForalls [kData, kRate]
         $  \[tA, tK] -> tSeries (tDown n tK) tA `tFun` tNat `tFun` tVec n tA

        -- down$N# :: [a : Data]. [k : Rate]
        --         .  Series# k a -> Series# (DownN# k) a
        OpSeriesDown n
         -> tForalls [kData, kRate]
         $  \[tA, tK] -> tSeries tK tA `tFun` tSeries (tDown n tK) tA

        -- tail$N# :: [a : Data]. [k : Rate]
        --         .  Series# k a -> Series# (TailN# k) a
        OpSeriesTail n
         -> tForalls [kData, kRate]
         $  \[tA, tK] -> tSeries tK tA `tFun` tSeries (tTail n tK) tA


-- Compounds ------------------------------------------------------------------
xRateOfSeries :: Type Name -> Type Name -> Exp () Name -> Exp () Name
xRateOfSeries tK tA xS 
         = xApps  (xVarOpSeries OpSeriesRateOfSeries) 
                  [XType tK, XType tA, xS]


xNatOfRateNat :: Type Name -> Exp () Name -> Exp () Name
xNatOfRateNat tK xR
        = xApps  (xVarOpSeries OpSeriesNatOfRateNat)
                 [XType tK, xR]


xNext  :: Type Name -> Type Name -> Exp () Name -> Exp () Name -> Exp () Name
xNext tRate tElem xStream xIndex
 = xApps (xVarOpSeries (OpSeriesNext 1))
         [XType tElem, XType tRate, xStream, xIndex]


xNextC :: Int -> Type Name -> Type Name -> Exp () Name -> Exp () Name -> Exp () Name
xNextC c tRate tElem xStream xIndex
 = xApps (xVarOpSeries (OpSeriesNext c))
         [XType tElem, XType tRate, xStream, xIndex]


-- Utils -----------------------------------------------------------------------
xVarOpSeries :: OpSeries -> Exp () Name
xVarOpSeries op
        = XVar  (UPrim (NameOpSeries op) (typeOpSeries op))

