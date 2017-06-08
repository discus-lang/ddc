
module DDC.Core.Flow.Transform.Schedule.Base
        ( elemBindOfSeriesBind
        , elemBoundOfSeriesBound
        , elemTypeOfSeriesType
        , resultRateTypeOfSeriesType
        , procTypeOfSeriesType

        , rateTypeOfRateVecType

        , elemTypeOfVectorType
        , bufOfVectorName)
where
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Exp


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
        | Just (_tcSeries, [_tP, _tK, tE]) <- takeTyConApps tSeries'
        = Just tE

        | otherwise
        = Nothing


-- | Given the type of a series like @Series p k l e@, produce the type
--   of the result rate, namely the @k@.
resultRateTypeOfSeriesType :: TypeF -> Maybe TypeF
resultRateTypeOfSeriesType tSeries'
        | isSeriesType tSeries'
        , Just (_tcSeries, [_tP, tK, _tE]) <- takeTyConApps tSeries'
        = Just tK

        | otherwise
        = Nothing

-- | Given the type of a series like @Series p k l e@, produce the type
--   of the process, namely the @p@
procTypeOfSeriesType :: TypeF -> Maybe TypeF
procTypeOfSeriesType tSeries'
        | isSeriesType tSeries'
        , Just (_tcSeries, [tP, _tK, _tE]) <- takeTyConApps tSeries'
        = Just tP

        | otherwise
        = Nothing


-- | Given the type of a rate-annotated vector like @RateVec k e@, produce the type
--   of the rate, namely the @k@.
rateTypeOfRateVecType :: TypeF -> Maybe TypeF
rateTypeOfRateVecType tV'
        | isRateVecType tV'
        , Just (_tcV, [tK, _tE]) <- takeTyConApps tV'
        = Just tK

        | otherwise
        = Nothing




-- Vector ---------------------------------------------------------------------
-- | Given the type of a vector like @Vector k e@, produce the type
--   of a single element, namely the @e@.
elemTypeOfVectorType :: TypeF -> Maybe TypeF
elemTypeOfVectorType tVector'
        | Just (_tcVector, [tE]) <- takeTyConApps tVector'
        = Just tE

        | otherwise
        = Nothing

-- | Given the name of a vector, find the name of the binding of its underlying buffer.
-- This binding is produced by Extract.
bufOfVectorName :: BoundF -> BoundF
bufOfVectorName (UName n) = UName $ NameVarMod n "buf"
bufOfVectorName b         = error (show b)

