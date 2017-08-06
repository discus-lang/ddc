{-# OPTIONS_HADDOCK hide #-}
module DDC.Core.Check.Error.ErrorData
        (ErrorData(..))
where
import DDC.Core.Exp


-- | Things that can go wrong when checking data type definitions.
data ErrorData n
        -- | A duplicate data type constructor name.
        = ErrorDataDupTypeName
        { errorDataDupTypeName          :: n }

        -- | A duplicate data constructor name.
        | ErrorDataDupCtorName
        { errorDataCtorName             :: n }

        -- | A data constructor with the wrong result type.
        | ErrorDataWrongResult
        { errorDataCtorName             :: n
        , errorDataCtorResultActual     :: Type n
        , errorDataCtorResultExpected   :: Type n }
        deriving Show
