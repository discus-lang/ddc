
module DDC.Core.Flow.Prim.OpFlow
        ( readOpFlow
        , typeOpFlow
        , xLengthOfRate)
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
        OpFlowToStream          -> text "toStream"              <> text "#"
        OpFlowFromStream        -> text "fromStream"            <> text "#"
        OpFlowLengthOfStream    -> text "lengthOfStream"        <> text "#"
        OpFlowToVector  n       -> text "toVector"   <> int n   <> text "#"
        OpFlowFromVector        -> text "fromVector"            <> text "#"
        OpFlowLengthOfRate      -> text "lengthOfRate"          <> text "#"
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


-- | Read a baked-in data flow operator.
readOpFlow :: String -> Maybe OpFlow
readOpFlow str
        | Just rest     <- stripPrefix "toVector" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ OpFlowToVector arity

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
                "toStream#"       -> Just $ OpFlowToStream
                "fromStream#"     -> Just $ OpFlowFromStream
                "lengthOfStream#" -> Just $ OpFlowLengthOfStream
                "toVector#"       -> Just $ OpFlowToVector 1
                "fromVector#"     -> Just $ OpFlowFromVector
                "lengthOfRate#"   -> Just $ OpFlowLengthOfRate
                "map#"            -> Just $ OpFlowMap 1
                "rep#"            -> Just $ OpFlowRep
                "reps#"           -> Just $ OpFlowReps
                "fold#"           -> Just $ OpFlowFold
                "folds#"          -> Just $ OpFlowFolds
                "unfold#"         -> Just $ OpFlowUnfold
                "unfolds#"        -> Just $ OpFlowUnfolds
                "pack#"           -> Just $ OpFlowPack
                _                 -> Nothing


-- Types -----------------------------------------------------------------------
-- | Take the type of a primitive data-flow operator.
typeOpFlow :: OpFlow -> Type Name
typeOpFlow op
 = case op of
        -- toStream   :: [k : Rate]. [a : Data]
        --            .  Vector k a -> Stream k a
        OpFlowToStream
         -> tForalls [kRate, kData]
         $  \[tK, tA]
         -> tVector tK tA `tFunPE` tStream tK tA

        -- fromStream :: [k : Rate]. [a : Data]
        --            .  Stream k a -> Vector k a
        OpFlowFromStream
         -> tForalls [kRate, kData]
         $  \[tK, tA]
         -> tStream tK tA `tFunPE` tVector tK tA

        -- fromVector :: [k : Rate]. [a : Data]
        --            .  Vector k a -> Array a
        OpFlowFromVector 
         -> tForalls [kRate, kData]
         $  \[tK, tA]
         -> tVector tK tA `tFunPE` tArray tA

        -- lengthOfRate# :: [k : Rate]. Nat#
        OpFlowLengthOfRate
         -> tForall kRate $ \_ -> tNat

        -- mkSel1#    :: [k1 : Rate]. [a : Data]
        --            .  Stream k1 Bool#
        --            -> ([k2 : Rate]. Sel1 k1 k2 -> a)
        --            -> a
        OpFlowMkSel 1
         -> tForalls [kRate, kData]
         $  \[tK1, tA]
         -> tStream tK1 tBool
                `tFunPE` (tForall kRate $ \tK2 
                                -> tSel1 (liftT 1 tK1) tK2 `tFunPE` (liftT 1 tA))
                `tFunPE` tA

        -- map   :: [k : Rate] [a b : Data]
        --       .  (a -> b) -> Stream k a -> Stream k b
        OpFlowMap 1
         -> tForalls [kRate, kData, kData]
         $  \[tK, tA, tB]
         -> (tA `tFunPE` tB)
                `tFunPE` tStream tK tA
                `tFunPE` tStream tK tB

        -- rep  :: [n : Nat']. [a : Data]
        --      .  n -> a -> Stream (Len n) a
        OpFlowRep 
         -> tForalls [kNatP, kData]
         $  \[tN, tA]
         ->     tN `tFunPE` tA `tFunPE` tStream (tLen tN) tA

        -- reps  :: [k1 k2 : Rate]. [a : Data]
        --       .  Segd   k1 k2 
        --       -> Stream k1 a
        --       -> Stream k2 a
        OpFlowReps 
         -> tForalls [kRate, kRate, kData]
         $  \[tK1, tK2, tA]
         -> tSegd tK1 tK2
                `tFunPE` tStream tK1 tA
                `tFunPE` tStream tK2 tA

        -- fold :: [k : Rate]. [a b: Data]
        --      .  (a -> b -> a) -> a -> Stream k b -> a
        OpFlowFold    
         -> tForalls [kRate, kData, kData] 
         $  \[tK, tA, tB]
         -> (tA `tFunPE` tB `tFunPE` tA)
                `tFunPE` tA
                `tFunPE` tStream tK tA
                `tFunPE` tA

        -- folds :: [k1 k2 : Rate]. [a b: Data]
        --       .  Segd   k1 k2 
        --       -> (a -> b -> a)       -- fold operator
        --       -> Stream k1 a         -- start values
        --       -> Stream k2 b         -- source elements
        --       -> Stream k1 a         -- result values
        OpFlowFolds
         -> tForalls [kRate, kRate, kData, kData]
         $  \[tK1, tK2, tA, tB]
         -> tSegd tK1 tK2
                `tFunPE` (tA `tFunPE` tB `tFunPE` tA)
                `tFunPE` tStream tK1 tA
                `tFunPE` tStream tK2 tB
                `tFunPE` tStream tK1 tA

        -- pack  :: [k1 k2 : Rate]. [a : Data]
        --       .  Sel2 k1 k2
        --       -> Stream k1 a -> Stream k2 a
        OpFlowPack
         -> tForalls [kRate, kRate, kData]
         $  \[tK1, tK2, tA]
         -> tSel1 tK1 tK2 
                `tFunPE` tStream tK1 tA
                `tFunPE` tStream tK2 tA

        _ -> error $ "typeOfPrimFlow: not finished for " ++ show op


-- Compounds ------------------------------------------------------------------
xLengthOfRate :: Type Name -> Exp () Name
xLengthOfRate t
 = XApp () (XVar () (UPrim (NameOpFlow OpFlowLengthOfRate)
                           (typeOpFlow OpFlowLengthOfRate)))
           (XType t)

