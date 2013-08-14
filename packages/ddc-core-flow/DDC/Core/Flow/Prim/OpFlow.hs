
module DDC.Core.Flow.Prim.OpFlow
        ( readOpFlow
        , typeOpFlow

          -- * Compounds
        , xProj)
where
import DDC.Core.Flow.Prim.KiConFlow
import DDC.Core.Flow.Prim.TyConFlow
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Flow.Prim.Base
import DDC.Core.Transform.LiftT
import DDC.Core.Compounds.Simple
import DDC.Core.Exp.Simple
import DDC.Base.Pretty
import Control.DeepSeq
import Data.List
import Data.Char        


instance NFData OpFlow


instance Pretty OpFlow where
 ppr pf
  = case pf of
        OpFlowProj n i          
         -> text "proj" <> int n <> text "_" <> int i           <> text "#"

        OpFlowMap 1             -> text "map"                   <> text "#"
        OpFlowMap i             -> text "map"        <> int i   <> text "#"

        OpFlowRep               -> text "rep"                   <> text "#"
        OpFlowReps              -> text "reps"                  <> text "#"

        OpFlowMkSel 1           -> text "mkSel"                 <> text "#"
        OpFlowMkSel n           -> text "mkSel"      <> int n   <> text "#"

        OpFlowPack              -> text "pack"                  <> text "#"

        OpFlowReduce            -> text "reduce"                <> text "#"
        OpFlowFold              -> text "fold"                  <> text "#"
        OpFlowFoldIndex         -> text "foldIndex"             <> text "#"
        OpFlowFolds             -> text "folds"                 <> text "#"

        OpFlowCreate            -> text "create"                <> text "#"
        OpFlowFill              -> text "fill"                  <> text "#"
        OpFlowGather            -> text "gather"                <> text "#"
        OpFlowScatter           -> text "scatter"               <> text "#"


-- | Read a data flow operator name.
readOpFlow :: String -> Maybe OpFlow
readOpFlow str
        | Just rest         <- stripPrefix "proj" str
        , (ds, '_' : rest2) <- span isDigit rest
        , not $ null ds
        , arity             <- read ds
        , arity >= 1
        , (ds2, "#")        <- span isDigit rest2
        , not $ null ds2
        , ix                <- read ds2
        , ix >= 1
        , ix <= arity
        = Just $ OpFlowProj arity ix

        | Just rest     <- stripPrefix "map" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ OpFlowMap arity

        | Just rest     <- stripPrefix "mkSel" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        , arity == 1
        = Just $ OpFlowMkSel arity

        | otherwise
        = case str of
                "map#"          -> Just $ OpFlowMap   1
                "rep#"          -> Just $ OpFlowRep
                "reps#"         -> Just $ OpFlowReps
                "mkSel#"        -> Just $ OpFlowMkSel 1
                "pack#"         -> Just $ OpFlowPack
                "reduce#"       -> Just $ OpFlowReduce
                "fold#"         -> Just $ OpFlowFold
                "foldIndex#"    -> Just $ OpFlowFoldIndex
                "folds#"        -> Just $ OpFlowFolds
                "create#"       -> Just $ OpFlowCreate
                "fill#"         -> Just $ OpFlowFill
                "gather#"       -> Just $ OpFlowGather
                "scatter#"      -> Just $ OpFlowScatter
                _               -> Nothing


-- Types -----------------------------------------------------------------------
-- | Yield the type of a data flow operator, 
--   or `error` if there isn't one.
typeOpFlow :: OpFlow -> Type Name
typeOpFlow op
 = case takeTypeOpFlow op of
        Just t  -> t
        Nothing -> error $ "ddc-core-flow.typeOpFlow: invalid op " ++ show op


-- | Yield the type of a data flow operator.
takeTypeOpFlow :: OpFlow -> Maybe (Type Name)
takeTypeOpFlow op
 = case op of
        -- Tuple projections --------------------
        OpFlowProj a ix
         -> Just $ tForalls (replicate a kData) 
         $ \_ -> tFun   (tTupleN [TVar (UIx i) | i <- reverse [0..a-1]])
                        (TVar (UIx (a - ix)))

        -- Maps ---------------------------------
        -- map   :: [k : Rate] [a b : Data]
        --       .  (a -> b) -> Series k a -> Series k b
        OpFlowMap 1
         -> Just $ tForalls [kRate, kData, kData] $ \[tK, tA, tB]
                ->       (tA `tFun` tB)
                `tFun` tSeries tK tA
                `tFun` tSeries tK tB

        -- mapN  :: [k : Rate] [a0..aN : Data]
        --       .  (a0 -> .. aN) -> Series k a0 -> .. Series k aN
        OpFlowMap n
         | n >= 2
         , Just tWork <- tFunOfList   
                         [ TVar (UIx i) 
                                | i <- reverse [0..n] ]

         , Just tBody <- tFunOfList
                         (tWork : [tSeries (TVar (UIx (n + 1))) (TVar (UIx i)) 
                                | i <- reverse [0..n] ])

         -> Just $ foldr TForall tBody
                         [ BAnon k | k <- kRate : replicate (n + 1) kData ]

        -- Replicates -------------------------
        -- rep  :: [a : Data] [k : Rate]
        --      .  a -> Series k a
        OpFlowRep 
         -> Just $ tForalls [kData, kRate] $ \[tA, tR]
                -> tA `tFun` tSeries tR tA

        -- reps  :: [k1 k2 : Rate]. [a : Data]
        --       .  Segd   k1 k2 
        --       -> Series k1 a
        --       -> Series k2 a
        OpFlowReps 
         -> Just $ tForalls [kRate, kRate, kData] $ \[tK1, tK2, tA]
                -> tSegd tK1 tK2 `tFun` tSeries tK1 tA `tFun` tSeries tK2 tA

        -- Selectors ----------------------------
        -- mkSel1#    :: [k1 : Rate]. [a : Data]
        --            .  Series k1 Bool#
        --            -> ([k2 : Rate]. Sel1 k1 k2 -> a)
        --            -> a
        OpFlowMkSel 1
         -> Just $ tForalls [kRate, kData] $ \[tK1, tA]
                ->       tSeries tK1 tBool
                `tFun` (tForall kRate $ \tK2 
                                -> tSel1 (liftT 1 tK1) tK2 `tFun` (liftT 1 tA))
                `tFun` tA

        -- Packs --------------------------------
        -- pack  :: [k1 k2 : Rate]. [a : Data]
        --       .  Sel2 k1 k2
        --       -> Series k1 a -> Series k2 a
        OpFlowPack
         -> Just $ tForalls [kRate, kRate, kData] $ \[tK1, tK2, tA]
                ->     tSel1   tK1 tK2 
                `tFun` tSeries tK1 tA `tFun` tSeries tK2 tA

        -- Reduce and Fold ----------------------
        -- reduce :: [k : Rate]. [a : Data]
        --        .  Ref a -> (a -> a -> a) -> a -> Series k a -> Unit
        OpFlowReduce
         -> Just $ tForalls [kRate, kData] $ \[tK, tA]
                 ->     tRef tA
                 `tFun` (tA `tFun` tA `tFun` tA)
                 `tFun` tA
                 `tFun` tSeries tK tA
                 `tFun` tUnit

        -- fold   :: [k : Rate]. [a b: Data]
        --        .  (a -> b -> a) -> a -> Series k b -> a
        OpFlowFold    
         -> Just $ tForalls [kRate, kData, kData] $ \[tK, tA, tB]
                ->     (tA `tFun` tB `tFun` tA)
                `tFun` tA
                `tFun` tSeries tK tB
                `tFun` tA

        -- foldIndex :: [k : Rate]. [a b: Data]
        --           .  (Nat# -> a -> b -> a) -> a -> Series k b -> a
        OpFlowFoldIndex
         -> Just $ tForalls [kRate, kData, kData] $ \[tK, tA, tB]
                 ->     (tNat `tFun` tA `tFun` tB `tFun` tA)
                 `tFun` tA
                 `tFun` tSeries tK tB
                 `tFun` tA

        -- folds :: [k1 k2 : Rate]. [a b: Data]
        --       .  Segd   k1 k2 
        --       -> (a -> b -> a)       -- fold operator
        --       -> Series k1 a         -- start values
        --       -> Series k2 b         -- source elements
        --       -> Series k1 a         -- result values
        OpFlowFolds
         -> Just $ tForalls [kRate, kRate, kData, kData] $ \[tK1, tK2, tA, tB]
                 ->      tSegd tK1 tK2
                 `tFun` (tInt `tFun` tA `tFun` tB `tFun` tA)
                 `tFun` tSeries tK1 tA `tFun` tSeries tK2 tB `tFun` tSeries tK1 tA

        -- Vector creation and filling ----------
        -- create#  :: [k : Rate]. [a : Data]. Series k a -> Vector a
        OpFlowCreate
         -> Just $ tForalls [kRate, kData] $ \[tK, tA] 
                -> tSeries tK tA `tFun` tVector tA

        -- fill#    :: [k : Rate]. [a : Data]. Vector a -> Series k a -> Unit
        OpFlowFill
         -> Just $ tForalls [kRate, kData] $ \[tK, tA] 
                -> tVector tA `tFun` tSeries tK tA `tFun` tUnit

        -- gather#  :: [k : Rate]. [a : Data]
        --          . Vector a -> Series k Nat# -> Series k a
        OpFlowGather
         -> Just $ tForalls [kRate, kData] $ \[tK, tA]
                 -> tVector tA 
                 `tFun` tSeries tK tNat `tFun` tSeries tK tA

        -- scatter# :: [k : Rate]. [a : Data]
        --          .  Vector a -> Series k Nat# -> Series k a -> Unit
        OpFlowScatter
         -> Just $ tForalls [kRate, kData] $ \[tK, tA]
                 -> tVector tA 
                 `tFun` tSeries tK tNat `tFun` tSeries tK tA `tFun` tUnit

        _ -> Nothing


-- Compounds ------------------------------------------------------------------
xProj :: [Type Name] -> Int -> Exp () Name -> Exp () Name
xProj ts ix  x
        = xApps   (xVarOpFlow (OpFlowProj (length ts) ix))
                  ([XType t | t <- ts] ++ [x])


-- Utils -----------------------------------------------------------------------
xVarOpFlow :: OpFlow -> Exp () Name
xVarOpFlow op
        = XVar  (UPrim (NameOpFlow op) (typeOpFlow op))

