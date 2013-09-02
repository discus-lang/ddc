
module DDC.Core.Flow.Prim.OpSeries
        ( readOpSeries
        , typeOpSeries

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


instance NFData OpSeries


instance Pretty OpSeries where
 ppr pf
  = case pf of
        OpSeriesProj n i          
         -> text "proj" <> int n <> text "_" <> int i             <> text "#"

        OpSeriesRep             -> text "srep"                  <> text "#"
        OpSeriesReps            -> text "sreps"                 <> text "#"
        OpSeriesGather          -> text "sgather"               <> text "#"

        OpSeriesMkSel 1         -> text "smkSel"                <> text "#"
        OpSeriesMkSel n         -> text "smkSel"     <> int n   <> text "#"

        OpSeriesMap 1           -> text "smap"                  <> text "#"
        OpSeriesMap i           -> text "smap"       <> int i   <> text "#"

        OpSeriesPack            -> text "spack"                 <> text "#"

        OpSeriesReduce          -> text "sreduce"               <> text "#"
        OpSeriesFill            -> text "sfill"                 <> text "#"
        OpSeriesScatter         -> text "sscatter"              <> text "#"

        OpSeriesJoin            -> text "pjoin"                 <> text "#"

        OpSeriesRunSeries 1       -> text "runSeries"             <> text "#"
        OpSeriesRunSeries n       -> text "runSeries"  <> int n   <> text "#"


-- | Read a data flow operator name.
readOpSeries :: String -> Maybe OpSeries
readOpSeries str
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
        = Just $ OpSeriesProj arity ix

        | Just rest     <- stripPrefix "smap" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ OpSeriesMap arity

        | Just rest     <- stripPrefix "smkSel" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        , arity == 1
        = Just $ OpSeriesMkSel arity

        | Just rest     <- stripPrefix "runSeries" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ OpSeriesRunSeries arity


        | otherwise
        = case str of
                "srep#"         -> Just $ OpSeriesRep
                "sreps#"        -> Just $ OpSeriesReps
                "sgather#"      -> Just $ OpSeriesGather
                "smkSel#"       -> Just $ OpSeriesMkSel 1
                "smap#"         -> Just $ OpSeriesMap   1
                "spack#"        -> Just $ OpSeriesPack
                "sreduce#"      -> Just $ OpSeriesReduce
                "sfill#"        -> Just $ OpSeriesFill
                "sscatter#"     -> Just $ OpSeriesScatter
                _               -> Nothing


-- Types -----------------------------------------------------------------------
-- | Yield the type of a data flow operator, 
--   or `error` if there isn't one.
typeOpSeries :: OpSeries -> Type Name
typeOpSeries op
 = case takeTypeOpSeries op of
        Just t  -> t
        Nothing -> error $ "ddc-core-flow.typeOpSeries: invalid op " ++ show op


-- | Yield the type of a data flow operator.
takeTypeOpSeries :: OpSeries -> Maybe (Type Name)
takeTypeOpSeries op
 = case op of
        -- Tuple projections --------------------
        OpSeriesProj a ix
         -> Just $ tForalls (replicate a kData) 
         $ \_ -> tFun   (tTupleN [TVar (UIx i) | i <- reverse [0..a-1]])
                        (TVar (UIx (a - ix)))


        -- Replicates -------------------------
        -- rep  :: [a : Data] [k : Rate]
        --      .  a -> Series k a
        OpSeriesRep 
         -> Just $ tForalls [kData, kRate] $ \[tA, tR]
                -> tA `tFun` tSeries tR tA

        -- reps  :: [k1 k2 : Rate]. [a : Data]
        --       .  Segd   k1 k2 
        --       -> Series k1 a
        --       -> Series k2 a
        OpSeriesReps 
         -> Just $ tForalls [kRate, kRate, kData] $ \[tK1, tK2, tA]
                -> tSegd tK1 tK2 `tFun` tSeries tK1 tA `tFun` tSeries tK2 tA


        -- Gather -------------------------------
        -- gather#  :: [k : Rate]. [a : Data]
        --          . Vector a -> Series k Nat# -> Series k a
        OpSeriesGather
         -> Just $ tForalls [kRate, kData] $ \[tK, tA]
                 -> tVector tA 
                 `tFun` tSeries tK tNat `tFun` tSeries tK tA

        -- Maps ---------------------------------
        -- map   :: [k : Rate] [a b : Data]
        --       .  (a -> b) -> Series k a -> Series k b
        OpSeriesMap 1
         -> Just $ tForalls [kRate, kData, kData] $ \[tK, tA, tB]
                ->       (tA `tFun` tB)
                `tFun` tSeries tK tA
                `tFun` tSeries tK tB

        -- mapN  :: [k : Rate] [a0..aN : Data]
        --       .  (a0 -> .. aN) -> Series k a0 -> .. Series k aN
        OpSeriesMap n
         | n >= 2
         , Just tWork <- tFunOfList   
                         [ TVar (UIx i) 
                                | i <- reverse [0..n] ]

         , Just tBody <- tFunOfList
                         (tWork : [tSeries (TVar (UIx (n + 1))) (TVar (UIx i)) 
                                | i <- reverse [0..n] ])

         -> Just $ foldr TForall tBody
                         [ BAnon k | k <- kRate : replicate (n + 1) kData ]


        -- Packs --------------------------------
        -- pack  :: [k1 k2 : Rate]. [a : Data]
        --       .  Sel2 k1 k2
        --       -> Series k1 a -> Series k2 a
        OpSeriesPack
         -> Just $ tForalls [kRate, kRate, kData] $ \[tK1, tK2, tA]
                ->     tSel1   tK1 tK2 
                `tFun` tSeries tK1 tA `tFun` tSeries tK2 tA


        -- Processes ----------------------------
        -- join#    :: Process -> Process -> Process
        OpSeriesJoin
         -> Just $ tProcess `tFun` tProcess `tFun` tProcess


        -- mkSel1#    :: [k1 : Rate].
        --            .  Series k1 Bool#
        --            -> ([k2 : Rate]. Sel1 k1 k2 -> Process#)
        --            -> Process#
        OpSeriesMkSel 1
         -> Just $ tForalls [kRate] $ \[tK1]
                ->       tSeries tK1 tBool
                `tFun` (tForall kRate $ \tK2 
                                -> tSel1 (liftT 1 tK1) tK2 `tFun` tProcess)
                `tFun` tProcess

        -- reduce# :: [k : Rate]. [a : Data]
        --        .  Ref a -> (a -> a -> a) -> a -> Series k a -> Process
        OpSeriesReduce
         -> Just $ tForalls [kRate, kData] $ \[tK, tA]
                 ->     tRef tA
                 `tFun` (tA `tFun` tA `tFun` tA)
                 `tFun` tA
                 `tFun` tSeries tK tA
                 `tFun` tProcess

        -- fill#    :: [k : Rate]. [a : Data]. Vector a -> Series k a -> Process
        OpSeriesFill
         -> Just $ tForalls [kRate, kData] $ \[tK, tA] 
                -> tVector tA `tFun` tSeries tK tA `tFun` tProcess

        -- scatter# :: [k : Rate]. [a : Data]
        --          .  Vector a -> Series k Nat# -> Series k a -> Process
        OpSeriesScatter
         -> Just $ tForalls [kRate, kData] $ \[tK, tA]
                 -> tVector tA 
                 `tFun` tSeries tK tNat `tFun` tSeries tK tA `tFun` tProcess


        -- OLD NEWS: delete me ===================
        -- create#  :: [k : Rate]. [a : Data]. Series k a -> Vector a
        OpSeriesCreate
         -> Just $ tForalls [kRate, kData] $ \[tK, tA] 
                -> tSeries tK tA `tFun` tVector tA

        -- fold   :: [k : Rate]. [a b: Data]
        --        .  (a -> b -> a) -> a -> Series k b -> a
        OpSeriesFold    
         -> Just $ tForalls [kRate, kData, kData] $ \[tK, tA, tB]
                ->     (tA `tFun` tB `tFun` tA)
                `tFun` tA
                `tFun` tSeries tK tB
                `tFun` tA

        -- foldIndex :: [k : Rate]. [a b: Data]
        --           .  (Nat# -> a -> b -> a) -> a -> Series k b -> a
        OpSeriesFoldIndex
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
        OpSeriesFolds
         -> Just $ tForalls [kRate, kRate, kData, kData] $ \[tK1, tK2, tA, tB]
                 ->      tSegd tK1 tK2
                 `tFun` (tInt `tFun` tA `tFun` tB `tFun` tA)
                 `tFun` tSeries tK1 tA `tFun` tSeries tK2 tB `tFun` tSeries tK1 tA


        _ -> Nothing


-- Compounds ------------------------------------------------------------------
xProj :: [Type Name] -> Int -> Exp () Name -> Exp () Name
xProj ts ix  x
        = xApps   (xVarOpSeries (OpSeriesProj (length ts) ix))
                  ([XType t | t <- ts] ++ [x])


-- Utils -----------------------------------------------------------------------
xVarOpSeries :: OpSeries -> Exp () Name
xVarOpSeries op
        = XVar  (UPrim (NameOpSeries op) (typeOpSeries op))

