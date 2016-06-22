
module DDC.Core.Flow.Transform.Concretize
        (concretizeModule)
where
import DDC.Core.Module
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Exp
import DDC.Core.Transform.TransformUpX
import DDC.Core.Env.EnvX                (EnvX)
import qualified DDC.Core.Env.EnvX      as EnvX
import qualified Data.Map               as Map


-- | Rewrite operators that use type level rates to ones that 
--   use value level ones.
concretizeModule :: Module () Name -> Module () Name
concretizeModule mm
        = transformSimpleUpX concretizeX EnvX.empty mm


-- | Rewrite an expression to use concrete operators.
concretizeX 
        :: EnvX Name
        -> ExpF     -> Maybe ExpF

concretizeX env xx

        -- loop# -> loopn#
        -- using an existing RateNat in the environment.
        | Just ( NameOpControl OpControlLoop
               , [XType tK, xF]) <- takeXPrimApps xx
        , Just nRN               <- findRateNatWithRate env tK
        , xRN                    <- XVar (UName nRN)
        = Just
        $ xLoopN tK xRN xF

        -- loop# -> loopn#
        -- using the length of a series to get the rate.
        | Just ( NameOpControl OpControlLoop
               , [XType tK, xF])   <- takeXPrimApps xx
        , Just (nS, tP, _, tA)     <- findSeriesWithRate env tK
        , xS                       <- XVar (UName nS)
        = Just 
        $ xLoopN 
                tK                              -- type level rate
                (xRateOfSeries tP tK tA xS)  -- 
                xF                              -- loop body

        -- newVectorR# -> newVector#
        | Just ( NameOpStore OpStoreNewVectorR
               , [XType tA, XType tK])  <- takeXPrimApps xx
        , Just (nS, tP, _, tS)          <- findSeriesWithRate env tK
        , xS                            <- XVar (UName nS)
        = Just
        $ xNewVector
                tA
                (xNatOfRateNat tK $ xRateOfSeries tP tK tS xS)
                
        | otherwise
        = Nothing


-------------------------------------------------------------------------------
-- | Search the given environment for the name of a RateNat with the
--   given rate parameter. We only look at named binders.
findRateNatWithRate 
        :: EnvX Name            -- ^ Type Environment.
        -> Type Name            -- ^ Rate type.
        -> Maybe Name
                                -- ^ RateNat name
findRateNatWithRate env tR
 = go (Map.toList (EnvX.envxMap env))
 where  go []           = Nothing
        go ((n, tRN) : moar)
         | isRateNatTypeOfRate tR tRN = Just n
         | otherwise                  = go moar


-- | Check whether some type is a RateNat type of the given rate.
isRateNatTypeOfRate 
        :: Type Name -> Type Name 
        -> Bool

isRateNatTypeOfRate tR tRN
        | Just ( NameTyConFlow TyConFlowRateNat
               , [tR'])    <- takePrimTyConApps tRN
        , tR == tR'
        = True

        | otherwise
        = False


-------------------------------------------------------------------------------
-- | Search the given environment for the name of a series with the
--   given result rate parameter. We only look at named binders.
findSeriesWithRate 
        :: EnvX Name             -- ^ Type Environment.
        -> Type Name            -- ^ Rate type.
        -> Maybe (Name, Type Name, Type Name, Type Name)
        -- ^ Series name, process, result rate, element type.
findSeriesWithRate env tK
 = go (Map.toList (EnvX.envxMap env))
 where  go []           = Nothing
        go ((n, tS) : moar)
         = case isSeriesTypeOfRate tK tS of
                Nothing              -> go moar
                Just (tP, _, tA) -> Just (n, tP, tK, tA)


-- | Given a rate type and a stream type, check whether the stream
--   is of the given result rate. If it is then return the process, result rate,
--   loop rate and element types, otherwise `Nothing`.
isSeriesTypeOfRate 
        :: Type Name -> Type Name 
        -> Maybe (Type Name, Type Name, Type Name)

isSeriesTypeOfRate tK tS
        | Just ( NameTyConFlow TyConFlowSeries
               , [tP, tK', tA])    <- takePrimTyConApps tS
        , tK == tK'
        = Just (tP, tK, tA)

        | otherwise
        = Nothing

