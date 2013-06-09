
module DDC.Core.Flow.Transform.Concretize
        (concretizeModule)
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Compounds
import DDC.Core.Transform.TransformUpX
import DDC.Type.Env                     (KindEnv, TypeEnv)
import qualified DDC.Type.Env           as Env
import qualified Data.Map               as Map


-- | Rewrite operators that use type level rates to ones that 
--   use value level ones.
concretizeModule :: Module () Name -> Module () Name
concretizeModule mm
        = transformUpX concretizeX Env.empty Env.empty mm


concretizeX 
        :: KindEnv Name -> TypeEnv Name 
        -> Exp () Name  -> Exp () Name

concretizeX _kenv tenv xx
        -- loop# -> loopn#
        | Just (NameOpLoop OpLoopLoop, [XType tK, xF])       
                                <- takeXPrimApps xx
        , Just (nS, _, tA)      <- findSeriesWithRate tenv tK
        , xS                    <- XVar () (UName nS)
        = xLoopLoopN 
                tK                              -- type level rate
                (xRateOfSeries tK tA xS)        -- 
                xF                              -- loop body

        -- newVectorR# -> newVectorN#
        | Just (NameOpStore OpStoreNewVectorR
                        , [XType tA, XType tK])
                                <- takeXPrimApps xx
        , Just (nS, _, tS)      <- findSeriesWithRate tenv tK
        , xS                    <- XVar () (UName nS)
        = xNewVectorN
                tA tK
                (xRateOfSeries tK tS xS)
                
        | otherwise
        = xx


-- | Search the given environment for the name of a series with the
--   given rate parameter. We only look at named binders.
findSeriesWithRate 
        :: TypeEnv Name         -- ^ Type Environment.
        -> Type Name            -- ^ Rate type.
        -> Maybe (Name, Type Name, Type Name)
                                -- ^ Series name, rate type, element type.
findSeriesWithRate tenv tR
 = go (Map.toList (Env.envMap tenv))
 where 
        go []           = Nothing
        go ((n, tS) : moar)
         = case isSeriesTypeOfRate tR tS of
                Nothing         -> go moar
                Just (_, tA)    -> Just (n, tR, tA)


-- | Given a rate type and a stream type, check whether the stream
--   is of the given rate. If it is then return the rate and element
--   types, otherwise `Nothing`.
isSeriesTypeOfRate 
        :: Type Name -> Type Name 
        -> Maybe (Type Name, Type Name)

isSeriesTypeOfRate tR tS
        |  Just (NameTyConFlow TyConFlowSeries, [tR', tA])
                <- takePrimTyConApps tS
        ,  tR == tR'
        =  Just (tR, tA)

        | otherwise
        =  Nothing

