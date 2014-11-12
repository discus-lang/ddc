
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
        OpSeriesCross           -> text "scross"                <> text "#"

        OpSeriesResizeProc      -> text "presize"               <> text "#"
        OpSeriesResizeId        -> text "rid"                   <> text "#"
        OpSeriesResizeAppL      -> text "rappl"                 <> text "#"
        OpSeriesResizeAppR      -> text "rappr"                 <> text "#"
        OpSeriesResizeApp       -> text "rapp"                  <> text "#"
        OpSeriesResizeSel1      -> text "rsel1"                 <> text "#"
        OpSeriesResizeSegd      -> text "rsegd"                 <> text "#"
        OpSeriesResizeCross     -> text "rcross"                <> text "#"


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

                "presize#"      -> Just $ OpSeriesResizeProc
                "rid#"          -> Just $ OpSeriesResizeId
                "rappl#"        -> Just $ OpSeriesResizeAppL
                "rappr#"        -> Just $ OpSeriesResizeAppR
                "rapp#"         -> Just $ OpSeriesResizeApp
                "rsel1#"        -> Just $ OpSeriesResizeSel1
                "rsegd#"        -> Just $ OpSeriesResizeSegd
                "rcross#"       -> Just $ OpSeriesResizeCross

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
        --      .  a -> Series p k a
        OpSeriesRep 
         -> Just $ tForalls [kProc, kRate, kData] $ \[tP, tR, tA]
                -> tA `tFun` tSeries tP tR tA

        -- reps  :: [p : Proc]. [k1 k2 : Rate]. [a : Data]
        --       .  Segd k1 k2 -> Series p k1 a -> Series p k2 a
        OpSeriesReps 
         -> Just $ tForalls [kProc, kRate, kRate, kData] $ \[tP, tK1, tK2, tA]
                -> tSegd tK1 tK2 `tFun` tSeries tP tK1 tA `tFun` tSeries tP tK2 tA


        -- Indices ------------------------------
        -- indices :: [p : Proc]. [k1 k2 : Rate]. 
        --         .  Segd k1 k2 -> Series p k2 k1 Nat
        OpSeriesIndices
         -> Just $ tForalls [kProc, kRate, kRate] $ \[tP, tK1, tK2]
                 -> tSegd tK1 tK2 `tFun` tSeries tP tK2 tNat


        -- Maps ---------------------------------
        -- map   :: [p : Proc]. [kR kL : Rate] [a b : Data]
        --       .  (a -> b) -> Series p kR kL a -> Series p kR kL b
        OpSeriesMap 1
         -> Just $ tForalls [kProc, kRate, kData, kData] $ \[tP, tKR, tA, tB]
                ->       (tA `tFun` tB)
                `tFun` tSeries tP tKR tA
                `tFun` tSeries tP tKR tB

        -- mapN  :: [p : Proc] [kR kL : Rate] [a0..aN : Data]
        --       .  (a0 -> .. aN) -> Series p kR kL a0 -> .. Series p kR kL aN
        OpSeriesMap n
         | n >= 2
         , Just tWork <- tFunOfList   
                         [ TVar (UIx i) 
                                | i <- reverse [0..n] ]

         , Just tBody <- tFunOfList
                         (tWork : [tSeries (TVar $ UIx $ n + 2) (TVar $ UIx $ n + 1)
                                           (TVar $ UIx   i) 
                                | i <- reverse [0..n] ])

         -> Just $ foldr TForall tBody
                         [ BAnon k | k <- kProc : kRate : kRate : replicate (n + 1) kData ]


        -- Packs --------------------------------
        -- pack  :: [p : Proc]. [k1 k2 kL : Rate]. [a : Data]
        --       .  Sel2 k1 k2
        --       -> Series p k1 kL a -> Series p k2 kL a
        OpSeriesPack
         -> Just $ tForalls [kProc, kRate, kRate, kData] $ \[tP, tK1, tK2, tA]
                ->     tSel1   tP tK1 tK2 
                `tFun` tSeries tP tK1 tA
                `tFun` tSeries tP tK2 tA


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
         -> Just $ tForalls [kProc, kRate] $ \[tP, tK1]
                ->       tSeries tP tK1 tBool
                `tFun` (tForall kRate $ \tK2 
                                -> tSel1 (liftT 1 tP) (liftT 1 tK1) tK2 `tFun` tProcess (liftT 1 tP) (liftT 1 tK1))
                `tFun` tProcess tP tK1


        -- mkSegd#  :: [p : Proc]. [k1 kL : Rate]
        --          .  Series# p k1 kL Nat#
        --          -> ([k2 : Rate]. Segd# k1 k2 -> Process# p kL)
        --          -> Process# p kL
        OpSeriesMkSegd
         -> Just $ tForalls [kProc, kRate] $ \[tP, tK1]
                ->      tSeries tP tK1 tNat
                `tFun` (tForall kRate $ \tK2
                                -> tSegd (liftT 1 tK1) tK2 `tFun` tProcess (liftT 1 tP) (liftT 1 tK1))
                `tFun` tProcess tP tK1


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
        --         .  RateVec k a -> Series p k a
        OpSeriesSeriesOfRateVec
         -> Just $ tForalls [kProc, kRate, kData] $ \[tP, tK, tA]
                -> tRateVec tK tA `tFun` tSeries tP tK tA

        -- sappend# :: [p : Proc]. [k1R k2R : Rate]. [a : Data]
        --          .  Series p k1R a -> Series p k2R a
        --          -> Series p (k1R + k2R) a
        OpSeriesAppend
         -> Just $ tForalls [kProc, kRate, kRate, kData] $
                \[tP, tK1, tK2, tA]
                -> tSeries tP tK1 tA
            `tFun` tSeries tP tK2 tA
            `tFun` tSeries tP (tRateAppend tK1 tK2) tA

        -- scross#  :: [p : Proc]. [kR kO : Rate]. [a b : Data]
        --          .  Series p kR a
        --          -> RateVec  kO b
        --          -> Series p (kR * kO) (a,b)
        OpSeriesCross
         -> Just $ tForalls [kProc, kRate, kRate, kData, kData] $
                \[tP, tKR, tKO, tA, tB]
                -> tSeries tP tKR tA
            `tFun` tRateVec   tKO tB
            `tFun` tSeries tP (tRateCross tKR tKO) (tTuple2 tA tB)


        -- generate# :: [p : Proc]. [k : Rate]. [a : Data]
        --        .  (Nat# -> a) -> Series p k a
        OpSeriesGenerate
         -> Just $ tForalls [kProc, kRate, kData] $ \[tP, tK, tA]
                 ->     (tNat `tFun` tA)
                 `tFun` tSeries tP tK tA

        -- Reductions -------------------------------
        -- reduce# :: [p : Proc]. [k : Rate]. [a : Data]
        --        .  Ref a -> (a -> a -> a) -> a -> Series p k a -> Process p k
        OpSeriesReduce
         -> Just $ tForalls [kProc, kRate, kData] $ \[tP, tK, tA]
                 ->     tRef tA
                 `tFun` (tA `tFun` tA `tFun` tA)
                 `tFun` tA
                 `tFun` tSeries  tP tK tA
                 `tFun` tProcess tP tK


        -- folds#   :: [p : Proc]. [k1 k2 : Rate]. [a : Data]
        --          .  Segd# k1 k2 -> Series p k1 a -> Series k2 b
        OpSeriesFolds
         -> Just $ tForalls [kProc, kRate, kRate, kData] $ \[tP, tK1, tK2, tA]
                 ->     tSegd      tK1 tK2
                 `tFun` tSeries tP tK1 tA
                 `tFun` tSeries tP tK2 tA


        -- Store operators ---------------------------
        -- scatter# :: [p : Proc]. [k : Rate]. [a : Data]
        --          .  Vector a -> Series p k Nat# -> Series p k a -> Process p k
        OpSeriesScatter
         -> Just $ tForalls [kProc, kRate, kData] $ \[tP, tK, tA]
                 ->     tVector  tA
                 `tFun` tSeries  tP tK tNat
                 `tFun` tSeries  tP tK tA
                 `tFun` tProcess tP tK


        -- gather#  :: [p : Proc]. [k1 k2 : Rate]. [a : Data]
        --          . RateVec k1 a -> Series p k2 Nat# -> Series p k2 a
        OpSeriesGather
         -> Just $ tForalls [kProc, kRate, kRate, kData] $ \[tP, tK1, tK2, tA]
                 ->     tRateVec   tK1     tA 
                 `tFun` tSeries tP tK2 tNat
                 `tFun` tSeries tP tK2 tA


        -- fill#    :: [p : Proc]. [k : Rate]. [a : Data]. Vector a -> Series p k a -> Process p k
        OpSeriesFill
         -> Just $ tForalls [kProc, kRate, kData] $ \[tP, tK, tA] 
                ->    tVector        tA
               `tFun` tSeries  tP tK tA
               `tFun` tProcess tP tK


        -- Resizing -----------------------

        -- presize#  :: [p : Proc]. [j k : Rate]
        --           .  Resize  p j k
        --           -> Process p j
        --           -> Process p   k
        OpSeriesResizeProc
         -> Just $ tForalls [kProc, kRate, kRate] $
                \[tP, tJ, tK]
                -> tResize  tP tJ tK
            `tFun` tProcess tP tJ
            `tFun` tProcess tP    tK

        -- rid#      :: [p : Proc]. [k : Rate]
        --           .  Resize  p k k
        OpSeriesResizeId
         -> Just $ tForalls [kProc, kRate] $
                \[tP, tK]
                -> tResize  tP tK tK

        -- rappl#    :: [p : Proc]. [k l : Rate]
        --           .  Resize p k (Append k l)
        OpSeriesResizeAppL
         -> Just $ tForalls [kProc, kRate, kRate] $
                \[tP, tK, tL]
                -> tResize tP tK (tRateAppend tK tL)

        -- rappr#    :: [p : Proc]. [k l : Rate]
        --           .  Resize p l (Append k l)
        OpSeriesResizeAppR
         -> Just $ tForalls [kProc, kRate, kRate] $
                \[tP, tK, tL]
                -> tResize tP tL (tRateAppend tK tL)


        -- rapp#     :: [p : Proc]. [k k' l l' : Rate]
        --           .  Resize         k            k'
        --           -> Resize           l             l'
        --           -> Resize (Append k l) (Append k' l')
        OpSeriesResizeApp
         -> Just $ tForalls [kProc, kRate, kRate, kRate, kRate] $
                \[tP, tK, tK', tL, tL']
                -> tResize tP              tK                  tK'
            `tFun` tResize tP                 tL                   tL'
            `tFun` tResize tP (tRateAppend tK tL) (tRateAppend tK' tL')

        -- rsel1#    :: [p : Proc]. [j k l : Rate]
        --           .  Sel1   p   k l
        --           -> Resize p j   l
        --           -> Resize p j k
        OpSeriesResizeSel1
         -> Just $ tForalls [kProc, kRate, kRate, kRate] $
                \[tP, tJ, tK, tL]
                -> tSel1   tP    tK tL
            `tFun` tResize tP tJ    tL
            `tFun` tResize tP tJ tK

        -- rsegd#    :: [p : Proc]. [j k l : Rate]
        --           .  Segd       k l
        --           -> Resize p j   l
        --           -> Resize p j k
        OpSeriesResizeSegd
         -> Just $ tForalls [kProc, kRate, kRate, kRate] $
                \[tP, tJ, tK, tL]
                -> tSegd         tK tL
            `tFun` tResize tP tJ    tL
            `tFun` tResize tP tJ tK

        -- rcross#   :: [p : Proc]. [j k l : Rate]
        --           .  Resize p j (Cross k l)
        --           -> Resize p j        k
        OpSeriesResizeCross
         -> Just $ tForalls [kProc, kRate, kRate, kRate] $
                \[tP, tJ, tK, tL]
                -> tResize tP tJ (tRateCross tK tL)
            `tFun` tResize tP tJ             tK



        _ -> Nothing
