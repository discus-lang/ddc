
module DDC.Core.Flow.Transform.Schedule.Base
        ( elemBindOfSeriesBind
        , elemBoundOfSeriesBound
        , elemTypeOfSeriesType
        , rateTypeOfSeriesType
        , slurpRateOfParamTypes
        , isSeriesType)
where
import DDC.Core.Flow.Transform.Schedule.Fail
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Exp
import Data.Maybe


-- | Given the bind of a series,  produce the bound that refers to the
--   next element of the series in its context.
elemBindOfSeriesBind   :: BindF  -> Maybe BindF
elemBindOfSeriesBind bSeries
        | BName nSeries tSeries' <- bSeries
        , nElem         <- NameVarMod nSeries "elem"
        , Just tElem    <- elemTypeOfSeriesType tSeries'
        = Just $ BName nElem tElem

        | otherwise
        = Nothing
 

-- | Given the bound of a series, produce the bound that refers to the
--   next element of the series in its context.
elemBoundOfSeriesBound :: BoundF -> Maybe BoundF
elemBoundOfSeriesBound uSeries
        | UName nSeries <- uSeries
        , nElem         <- NameVarMod nSeries "elem"
        = Just $ UName nElem

        | otherwise
        = Nothing


-- | Given the type of a series like @Series k e@, produce the type
--   of a single element, namely the @e@.
elemTypeOfSeriesType :: TypeF -> Maybe TypeF
elemTypeOfSeriesType tSeries'
        | Just (_tcSeries, [_tK, tE]) <- takeTyConApps tSeries'
        = Just tE

        | otherwise
        = Nothing


-- | Given the type of a series like @Series k e@, produce the type
--   of the rate, namely the @k@.
rateTypeOfSeriesType :: TypeF -> Maybe TypeF
rateTypeOfSeriesType tSeries'
        | Just (_tcSeries, [tK, _tE]) <- takeTyConApps tSeries'
        = Just tK

        | otherwise
        = Nothing


-- | Given the type of the process parameters, 
--   yield the rate of the overall process.
slurpRateOfParamTypes :: [Type Name] -> Either Fail (Type Name)
slurpRateOfParamTypes tsParam
 = case mapMaybe rateTypeOfSeriesType tsParam of
        []                      -> Left FailNoSeriesParameters
        [tK]                    -> Right tK
        (tK : ts)
         | all (== tK) ts       -> Right tK
         | otherwise            -> Left FailMultipleRates


isSeriesType :: TypeF -> Bool
isSeriesType tt
 = case takePrimTyConApps tt of
        Just (NameTyConFlow TyConFlowSeries, [_, _]) -> True
        _                                            -> False

