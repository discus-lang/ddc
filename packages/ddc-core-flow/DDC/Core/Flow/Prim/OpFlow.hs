
module DDC.Core.Flow.Prim.OpFlow
        ( readOpFlow
        , typeOpFlow
        , xRateOfStream)
where
import DDC.Core.Flow.Prim.KiConFlow
import DDC.Core.Flow.Prim.TyConFlow
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Flow.Prim.Base
import DDC.Core.Transform.LiftT
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Base.Pretty
import Control.DeepSeq
import Data.List
import Data.Char        


instance NFData OpFlow


instance Pretty OpFlow where
 ppr pf
  = case pf of
        OpFlowVectorOfSeries    -> text "vectorOfSeries"        <> text "#"
        OpFlowRateOfSeries      -> text "rateOfSeries"          <> text "#"

        OpFlowMkSel n           -> text "mkSel"      <> int n   <> text "#"

        OpFlowMap i             -> text "map"        <> int i   <> text "#"

        OpFlowRep               -> text "rep"                   <> text "#"
        OpFlowReps              -> text "reps"                  <> text "#"

        OpFlowFold              -> text "fold"                  <> text "#"
        OpFlowFolds             -> text "folds"                 <> text "#"

        OpFlowUnfold            -> text "unfold"                <> text "#"
        OpFlowUnfolds           -> text "unfolds"               <> text "#"

        OpFlowSplit   i         -> text "split"      <> int i   <> text "#"
        OpFlowCombine i         -> text "combine"    <> int i   <> text "#"

        OpFlowPack              -> text "pack"                  <> text "#"


-- | Read a data flow operator name.
readOpFlow :: String -> Maybe OpFlow
readOpFlow str
        | Just rest     <- stripPrefix "mkSel" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ OpFlowMkSel arity

        | Just rest     <- stripPrefix "map" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ OpFlowMap arity

        | Just rest     <- stripPrefix "split" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ OpFlowSplit arity

        | Just rest     <- stripPrefix "combine" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ OpFlowCombine arity

        | otherwise
        = case str of
                "vectorOfSeries#"  -> Just $ OpFlowVectorOfSeries
                "rateOfSeries#"    -> Just $ OpFlowRateOfSeries
                "map#"             -> Just $ OpFlowMap 1
                "rep#"             -> Just $ OpFlowRep
                "reps#"            -> Just $ OpFlowReps
                "fold#"            -> Just $ OpFlowFold
                "folds#"           -> Just $ OpFlowFolds
                "unfold#"          -> Just $ OpFlowUnfold
                "unfolds#"         -> Just $ OpFlowUnfolds
                "pack#"            -> Just $ OpFlowPack
                _                  -> Nothing


-- Types -----------------------------------------------------------------------
-- | Yield the type of a data flow operator.
typeOpFlow :: OpFlow -> Type Name
typeOpFlow op
 = case op of
        -- Series Conversions -------------------
        -- vectorOfSeries# :: [k : Rate]. [a : Data]
        --                 .  Series k a -> Vector a
        OpFlowVectorOfSeries
         -> tForalls [kRate, kData]
         $  \[tK, tA] -> tSeries tK tA `tFunPE` tVector tA

        -- rateOfSeries#   :: [k : Rate]. [a : Data]
        --                 .  Series k a -> RateNat k
        OpFlowRateOfSeries 
         -> tForalls [kRate, kData]
         $  \[tK, tA]
                -> tSeries tK tA `tFunPE` tRateNat tK


        -- Selectors ----------------------------
        -- mkSel1#    :: [k1 : Rate]. [a : Data]
        --            .  Series k1 Bool#
        --            -> ([k2 : Rate]. Sel1 k1 k2 -> a)
        --            -> a
        OpFlowMkSel 1
         -> tForalls [kRate, kData]
         $  \[tK1, tA]
         -> tSeries tK1 tBool
                `tFunPE` (tForall kRate $ \tK2 
                                -> tSel1 (liftT 1 tK1) tK2 `tFunPE` (liftT 1 tA))
                `tFunPE` tA

        -- Maps ---------------------------------
        -- map   :: [k : Rate] [a b : Data]
        --       .  (a -> b) -> Series k a -> Series k b
        OpFlowMap 1
         -> tForalls [kRate, kData, kData]
         $  \[tK, tA, tB]
         -> (tA `tFunPE` tB)
                `tFunPE` tSeries tK tA
                `tFunPE` tSeries tK tB


        -- Replicates -------------------------
        -- rep  :: [a : Data] [k : Rate]
        --      .  a -> Series k a
        OpFlowRep 
         -> tForalls [kData, kRate]
         $  \[tA, tR]
         ->     tA `tFunPE` tSeries tR tA


        -- reps  :: [k1 k2 : Rate]. [a : Data]
        --       .  Segd   k1 k2 
        --       -> Series k1 a
        --       -> Series k2 a
        OpFlowReps 
         -> tForalls [kRate, kRate, kData]
         $  \[tK1, tK2, tA]
         -> tSegd tK1 tK2
                `tFunPE` tSeries tK1 tA
                `tFunPE` tSeries tK2 tA

        -- fold :: [k : Rate]. [a b: Data]
        --      .  (a -> b -> a) -> a -> Series k b -> a
        OpFlowFold    
         -> tForalls [kRate, kData, kData] 
         $  \[tK, tA, tB]
         -> (tA `tFunPE` tB `tFunPE` tA)
                `tFunPE` tA
                `tFunPE` tSeries tK tA
                `tFunPE` tA

        -- folds :: [k1 k2 : Rate]. [a b: Data]
        --       .  Segd   k1 k2 
        --       -> (a -> b -> a)       -- fold operator
        --       -> Series k1 a         -- start values
        --       -> Series k2 b         -- source elements
        --       -> Series k1 a         -- result values
        OpFlowFolds
         -> tForalls [kRate, kRate, kData, kData]
         $  \[tK1, tK2, tA, tB]
         -> tSegd tK1 tK2
                `tFunPE` (tA `tFunPE` tB `tFunPE` tA)
                `tFunPE` tSeries tK1 tA
                `tFunPE` tSeries tK2 tB
                `tFunPE` tSeries tK1 tA

        -- pack  :: [k1 k2 : Rate]. [a : Data]
        --       .  Sel2 k1 k2
        --       -> Series k1 a -> Series k2 a
        OpFlowPack
         -> tForalls [kRate, kRate, kData]
         $  \[tK1, tK2, tA]
         -> tSel1 tK1 tK2 
                `tFunPE` tSeries tK1 tA
                `tFunPE` tSeries tK2 tA

        _ -> error $ "typeOpFlow: not finished for " ++ show op


-- Compounds ------------------------------------------------------------------
xRateOfStream :: Type Name -> Type Name -> Exp () Name -> Exp () Name
xRateOfStream tK tA xS 
         = xApps () (xVarOpFlow OpFlowRateOfSeries) 
                    [XType tK, XType tA, xS]


-- Utils -----------------------------------------------------------------------
xVarOpFlow :: OpFlow -> Exp () Name
xVarOpFlow op
        = XVar () (UPrim (NameOpFlow op) (typeOpFlow op))

