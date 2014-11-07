
module DDC.Core.Flow.Prim.OpSeries
        ( readOpSeries
        , typeOpSeries)
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
        OpSeriesRep             -> text "srep"                  <> text "#"
        OpSeriesReps            -> text "sreps"                 <> text "#"

        OpSeriesIndices         -> text "sindices"              <> text "#"

        OpSeriesFill            -> text "sfill"                 <> text "#"

        OpSeriesGather          -> text "sgather"               <> text "#"
        OpSeriesScatter         -> text "sscatter"              <> text "#"

        OpSeriesMkSel 1         -> text "smkSel"                <> text "#"
        OpSeriesMkSel n         -> text "smkSel"     <> int n   <> text "#"

        OpSeriesMkSegd          -> text "smkSegd"               <> text "#"

        OpSeriesMap 1           -> text "smap"                  <> text "#"
        OpSeriesMap i           -> text "smap"       <> int i   <> text "#"

        OpSeriesPack            -> text "spack"                 <> text "#"

        OpSeriesGenerate        -> text "sgenerate"             <> text "#"

        OpSeriesReduce          -> text "sreduce"               <> text "#"
        OpSeriesFolds           -> text "sfolds"                <> text "#"

        OpSeriesJoin            -> text "pjoin"                 <> text "#"

        OpSeriesRunProcess      -> text "runProcess"            <> text "#"

        OpSeriesRateVecsOfVectors n -> text "ratify"   <> int n <> text "#"

        OpSeriesSeriesOfRateVec -> text "series"                <> text "#"
        OpSeriesAppend          -> text "sappend"               <> text "#"

        OpSeriesInjectLeft      -> text "sinjectL"              <> text "#"
        OpSeriesInjectRight     -> text "sinjectR"              <> text "#"
        OpSeriesCross           -> text "scross"                <> text "#"


-- | Read a data flow operator name.
readOpSeries :: String -> Maybe OpSeries
readOpSeries str
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

        | Just rest     <- stripPrefix "ratify" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ OpSeriesRateVecsOfVectors arity


        | otherwise
        = case str of
                "srep#"         -> Just $ OpSeriesRep
                "sreps#"        -> Just $ OpSeriesReps
                "sindices#"     -> Just $ OpSeriesIndices
                "sgather#"      -> Just $ OpSeriesGather
                "smkSel#"       -> Just $ OpSeriesMkSel 1
                "smkSegd#"      -> Just $ OpSeriesMkSegd
                "smap#"         -> Just $ OpSeriesMap   1
                "spack#"        -> Just $ OpSeriesPack
                "sgenerate#"    -> Just $ OpSeriesGenerate
                "sreduce#"      -> Just $ OpSeriesReduce
                "sfolds#"       -> Just $ OpSeriesFolds
                "sfill#"        -> Just $ OpSeriesFill
                "sscatter#"     -> Just $ OpSeriesScatter
                "pjoin#"        -> Just $ OpSeriesJoin
                "runProcess#"   -> Just $ OpSeriesRunProcess
                "series#"       -> Just $ OpSeriesSeriesOfRateVec
                "sappend#"      -> Just $ OpSeriesAppend
                "scross#"       -> Just $ OpSeriesCross
                "sinjectL#"     -> Just $ OpSeriesInjectLeft
                "sinjectR#"     -> Just $ OpSeriesInjectRight
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
        -- Replicates -------------------------
        -- rep  :: [p : Proc] [k : Rate] [a : Data] 
        --      .  a -> Series p k k a
        OpSeriesRep 
         -> Just $ tForalls [kProc, kRate, kData] $ \[tP, tR, tA]
                -> tA `tFun` tSeries tP tR tR tA

        -- reps  :: [p : Proc]. [k1 k2 kL : Rate]. [a : Data]
        --       .  Segd k1 k2 -> Series p k1 kL a -> Series p k2 kL a
        OpSeriesReps 
         -> Just $ tForalls [kProc, kRate, kRate, kRate, kData] $ \[tP, tK1, tK2, tKL, tA]
                -> tSegd tK1 tK2 `tFun` tSeries tP tK1 tKL tA `tFun` tSeries tP tK2 tKL tA


        -- Indices ------------------------------
        -- indices :: [p : Proc]. [k1 k2 : Rate]. 
        --         .  Segd k1 k2 -> Series p k2 k1 Nat
        OpSeriesIndices
         -> Just $ tForalls [kProc, kRate, kRate] $ \[tP, tK1, tK2]
                 -> tSegd tK1 tK2 `tFun` tSeries tP tK2 tK1 tNat


        -- Maps ---------------------------------
        -- map   :: [p : Proc]. [kR kL : Rate] [a b : Data]
        --       .  (a -> b) -> Series p kR kL a -> Series p kR kL b
        OpSeriesMap 1
         -> Just $ tForalls [kProc, kRate, kRate, kData, kData] $ \[tP, tKR, tKL, tA, tB]
                ->       (tA `tFun` tB)
                `tFun` tSeries tP tKR tKL tA
                `tFun` tSeries tP tKR tKL tB

        -- mapN  :: [p : Proc] [kR kL : Rate] [a0..aN : Data]
        --       .  (a0 -> .. aN) -> Series p kR kL a0 -> .. Series p kR kL aN
        OpSeriesMap n
         | n >= 2
         , Just tWork <- tFunOfList   
                         [ TVar (UIx i) 
                                | i <- reverse [0..n] ]

         , Just tBody <- tFunOfList
                         (tWork : [tSeries (TVar $ UIx $ n + 3) (TVar $ UIx $ n + 2)
                                           (TVar $ UIx $ n + 1) (TVar $ UIx   i) 
                                | i <- reverse [0..n] ])

         -> Just $ foldr TForall tBody
                         [ BAnon k | k <- kProc : kRate : kRate : replicate (n + 1) kData ]


        -- Packs --------------------------------
        -- pack  :: [p : Proc]. [k1 k2 kL : Rate]. [a : Data]
        --       .  Sel2 k1 k2
        --       -> Series p k1 kL a -> Series p k2 kL a
        OpSeriesPack
         -> Just $ tForalls [kProc, kRate, kRate, kRate, kData] $ \[tP, tK1, tK2, tKL, tA]
                ->     tSel1   tP tK1 tK2 
                `tFun` tSeries tP tK1 tKL tA
                `tFun` tSeries tP tK2 tKL tA


        -- Processes ----------------------------
        -- join#    :: [p : Proc]. [k : Rate]. [a b : Data].
        --          .  Process p k a
        --          -> Process p k b
        --          -> Process p k (a,b)
        OpSeriesJoin
         -> Just $ tForalls [kProc, kRate] $
                \[tP, tK]
                ->     tProcess tP tK
                `tFun` tProcess tP tK
                `tFun` tProcess tP tK


        -- mkSel1#  :: [p : Proc]. [k1 kL : Rate]
        --          .  Series p k1 kL Bool#
        --          -> ([k2 : Rate]. Sel1 p k1 k2 -> Process# p kL)
        --          -> Process# p kL
        OpSeriesMkSel 1
         -> Just $ tForalls [kProc, kRate, kRate] $ \[tP, tK1, tKL]
                ->       tSeries tP tK1 tKL tBool
                `tFun` (tForall kRate $ \tK2 
                                -> tSel1 (liftT 1 tP) (liftT 1 tK1) tK2 `tFun` tProcess (liftT 1 tP) (liftT 1 tKL))
                `tFun` tProcess tP tKL


        -- mkSegd#  :: [p : Proc]. [k1 kL : Rate]
        --          .  Series# p k1 kL Nat#
        --          -> ([k2 : Rate]. Segd# k1 k2 -> Process# p kL)
        --          -> Process# p kL
        OpSeriesMkSegd
         -> Just $ tForalls [kProc, kRate, kRate] $ \[tP, tK1, tKL]
                ->      tSeries tP tK1 tKL tNat
                `tFun` (tForall kRate $ \tK2
                                -> tSegd (liftT 1 tK1) tK2 `tFun` tProcess (liftT 1 tP) (liftT 1 tKL))
                `tFun` tProcess tP tKL


        -- runProcess# :: [k : Rate] [a : Data]
        --          .  
        --             ([p : Proc]. Unit -> Process p k a)
        --          ->  a
        OpSeriesRunProcess
         -> Just $ tForalls [kRate] $ \[tK]
                -> (tForall kProc $ \tP
                        -> tUnit `tFun` tProcess tP (liftT 1 tK))
                   `tFun` tUnit

        -- ratify0# :: [z : Data]
        --          .  Nat
        --          -> ([k : Rate]. z)
        --          -> z
        OpSeriesRateVecsOfVectors 0
         -> Just $ tForall kData $ \tA
                -> tNat
            `tFun` (tForall kRate $ \_ -> liftT 1 tA)
            `tFun` tA

        -- ratifyN# :: [a0..aN z : Data]
        --          .  Vector    a0 .. Vector   aN 
        --          -> ([k : Rate]. RateVec k a0 .. RateVec k aN -> z)
        --          -> z
        OpSeriesRateVecsOfVectors n
         | tK         <- TVar (UIx 0)

         , Just tWork <- tFunOfList   
                       $ [ tRateVec tK (TVar (UIx i))
                                | i <- reverse [2..n+1] ]
                       ++[ TVar (UIx 1) ]

         , tWork'     <- TForall (BAnon kRate) tWork

         , Just tBody <- tFunOfList
                         $ [ tVector (TVar (UIx i)) | i <- reverse [1..n] ]
                         ++[ tWork', TVar (UIx 0) ]

         -> Just $ foldr TForall tBody
                         [ BAnon k | k <- replicate (n+1) kData ]

        -- series# :: [p : Proc]. [k : Rate]. [a : Data]
        --         .  RateVec k a -> Series p k k a
        OpSeriesSeriesOfRateVec
         -> Just $ tForalls [kProc, kRate, kData] $ \[tP, tK, tA]
                -> tRateVec tK tA `tFun` tSeries tP tK tK tA

        -- sappend# :: [p : Proc]. [k1R k1L k2R k2L : Rate]. [a : Data]
        --          .  Series p k1R k1L a -> Series p k2R k2L a
        --          -> Series p (k1R + k2R) (k1L + k2L) a
        OpSeriesAppend
         -> Just $ tForalls [kProc, kRate, kRate, kRate, kRate, kData] $
                \[tP, tK1R, tK1L, tK2R, tK2L, tA]
                -> tSeries tP tK1R tK1L tA
            `tFun` tSeries tP tK2R tK2L tA
            `tFun` tSeries tP (tRatePlus tK1R tK2R) (tRatePlus tK1L tK2L) tA

        -- sinjectL# :: [p : Proc]. [kR kL kO : Rate]. [a : Data]
        --           .  Series p kR kL a
        --           -> Series p kR (kO + kL) a
        OpSeriesInjectLeft
         -> Just $ tForalls [kProc, kRate, kRate, kRate, kData] $
                \[tP, tKR, tKL, tKO, tA]
                -> tSeries tP tKR tKL tA
            `tFun` tSeries tP tKR (tRatePlus tKO tKL) tA

        -- sinjectR# :: [p : Proc]. [kR kL kO : Rate]. [a : Data]
        --           .  Series p kR kL a
        --           -> Series p kR (kL + kO) a
        OpSeriesInjectRight
         -> Just $ tForalls [kProc, kRate, kRate, kRate, kData] $
                \[tP, tKR, tKL, tKO, tA]
                -> tSeries tP tKR tKL tA
            `tFun` tSeries tP tKR (tRatePlus tKL tKO) tA

        -- scross#  :: [p : Proc]. [kR kL kO : Rate]. [a b : Data]
        --          .  Series p kR kL a
        --          -> RateVec     kO b
        --          -> Series p (kR * kO) kL (a,b)
        OpSeriesCross
         -> Just $ tForalls [kProc, kRate, kRate, kRate, kData, kData] $
                \[tP, tKR, tKL, tKO, tA, tB]
                -> tSeries tP tKR tKL tA
            `tFun` tRateVec       tKO tB
            `tFun` tSeries tP (tRateTimes tKR tKO) tKL (tTuple2 tA tB)


        -- generate# :: [p : Proc]. [k : Rate]. [a : Data]
        --        .  (Nat# -> a) -> Series p k k a
        OpSeriesGenerate
         -> Just $ tForalls [kProc, kRate, kData] $ \[tP, tK, tA]
                 ->     (tNat `tFun` tA)
                 `tFun` tSeries tP tK tK tA

        -- Reductions -------------------------------
        -- reduce# :: [p : Proc]. [kR kL : Rate]. [a : Data]
        --        .  Ref a -> (a -> a -> a) -> a -> Series p kR kL a -> Process p kL
        OpSeriesReduce
         -> Just $ tForalls [kProc, kRate, kRate, kData] $ \[tP, tKR, tKL, tA]
                 ->     tRef tA
                 `tFun` (tA `tFun` tA `tFun` tA)
                 `tFun` tA
                 `tFun` tSeries  tP tKR tKL tA
                 `tFun` tProcess tP tKL


        -- folds#   :: [p : Proc]. [k1 k2 kL : Rate]. [a : Data]
        --          .  Segd# k1 k2 -> Series p k1 kL a -> Series k2 b
        OpSeriesFolds
         -> Just $ tForalls [kProc, kRate, kRate, kRate, kData] $ \[tP, tK1, tK2, tKL, tA]
                 ->     tSegd      tK1 tK2
                 `tFun` tSeries tP tK1 tKL tA
                 `tFun` tSeries tP tK2 tKL tA


        -- Store operators ---------------------------
        -- scatter# :: [p : Proc]. [k kL : Rate]. [a : Data]
        --          .  Vector a -> Series p k kL Nat# -> Series p k kL a -> Process p kL
        OpSeriesScatter
         -> Just $ tForalls [kProc, kRate, kRate, kData] $ \[tP, tK, tKL, tA]
                 ->     tVector  tA
                 `tFun` tSeries  tP tK tKL tNat
                 `tFun` tSeries  tP tK tKL tA
                 `tFun` tProcess tP tKL


        -- gather#  :: [p : Proc]. [k1 k2 kL : Rate]. [a : Data]
        --          . RateVec k1 a -> Series p k2 kL Nat# -> Series p k2 kL a
        OpSeriesGather
         -> Just $ tForalls [kProc, kRate, kRate, kRate, kData] $ \[tP, tK1, tK2, tKL, tA]
                 ->     tRateVec    tK1     tA 
                 `tFun` tSeries tP tK2 tKL tNat
                 `tFun` tSeries tP tK2 tKL tA


        -- fill#    :: [p : Proc]. [k kL : Rate]. [a : Data]. Series p k kL a -> Process p kL (Vector k a)
        OpSeriesFill
         -> Just $ tForalls [kProc, kRate, kRate, kData] $ \[tP, tK, tKL, tA] 
                ->    tVector            tA
               `tFun` tSeries  tP tK tKL tA
               `tFun` tProcess tP tKL

        _ -> Nothing
