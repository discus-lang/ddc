
module DDC.Core.Flow.Process.Operator
        (Operator (..))
where
import DDC.Core.Exp
import DDC.Core.Flow.Prim


-- | An abstract series operator.
--
--   Each of the constructors holds all the information we need to produce
--   code for that operator.
data Operator
        -----------------------------------------
        -- | Convert a series to a manifest vector.
        = OpCreate
        { -- | Binder for result vector
          opResultVector        :: Bind Name

          -- | Rate of input series
        , opInputRate           :: Type Name

          -- | Bound of input series.
        , opInputSeries         :: Bound Name

          -- | Type of the elements.
        , opElemType            :: Type Name
        }

        -----------------------------------------
        -- | Apply a function to corresponding elements in several input series
        --   of the same rate, producing a new series. This subsumes the regular
        --   'map' operator as well as 'zipWith' like operators where the input
        --   lengths are identical.
        | OpMap
        { -- | Arity of map, number of input streams.
          opArity               :: Int

          -- | Binder for result series.
        , opResultSeries        :: Bind Name

          -- | Rate of all input series.
        , opInputRate           :: Type Name

          -- | Names for input series.
        , opInputSeriess        :: [Bound Name]

          -- | Worker input parameters
        , opWorkerParams        :: [Bind Name]

          -- | Worker body
        , opWorkerBody          :: Exp () Name
        }

        -----------------------------------------
        -- | Fold all the elements of a series.
        | OpFold
        { -- | Binder for result value.
          opResultValue         :: Bind Name

          -- | Rate of input series.
        , opInputRate           :: Type Name

          -- | Bound of input series.
        , opInputSeries         :: Bound Name

          -- | Starting accumulator value.
        , opZero                :: Exp () Name

          -- | Worker parameter for accumulator input.
        , opWorkerParamAcc      :: Bind Name

          -- | Worker parameter for element input.
        , opWorkerParamElem     :: Bind Name

          -- | Worker body.
        , opWorkerBody          :: Exp () Name }

        deriving (Show)
