
module DDC.Core.Flow.Process.Operator
        (Operator (..))
where
import DDC.Core.Flow.Exp


-- | An abstract series operator.
--
--   Each of the constructors holds all the information we need to produce
--   code for that operator.
data Operator
        -----------------------------------------
        -- | Connect a series from one place to another.
        --   These don't come from the source program, but are useful for 
        --   during code generation.
        = OpId
        { -- Binder for result series.
          opResultSeries        :: BindF

          -- Rate of the input series.
        , opInputRate           :: TypeF

          -- Bound of the input series
        , opInputSeries         :: BoundF

          -- Type of the elements.
        , opElemType            :: TypeF
        }

        -----------------------------------------
        -- | Convert a series to a manifest vector.
        | OpCreate
        { -- | Binder for result vector
          opResultVector        :: BindF

          -- | Rate of input series
        , opInputRate           :: TypeF

          -- | Bound of input series.
        , opInputSeries         :: BoundF

          -- | Rate that should be used when allocating the vector.
          --   This is filled in by `patchAllocRates`.
        , opAllocRate           :: Maybe TypeF

          -- | Type of the elements.
        , opElemType            :: TypeF
        }

        -----------------------------------------
        -- | Fill a vector with elements from a series.
        | OpFill
        { -- | Binder for result value (a Unit)
          opResultBind          :: BindF

          -- | Bound of target vector.
        , opTargetVector        :: BoundF

          -- | Rate of input series.
        , opInputRate           :: TypeF

          -- | Bound of input series.
        , opInputSeries         :: BoundF 

          -- | Type of the elements.
        , opElemType            :: TypeF }

        -----------------------------------------
        -- | Gather elements from a vector into a series.
        | OpGather
        { -- | Binder for result series.
          opResultBind          :: BindF

          -- | Bound  of source elem vector.
        , opSourceVector        :: BoundF

          -- | Bound  of source index series.
        , opSourceIndices       :: BoundF

          -- | Rate of input and output series.
        , opInputRate           :: TypeF

          -- | Type of gathered elements.
        , opElemType            :: TypeF 
        }

        -----------------------------------------
        -- | Scatter elements from a series into a vector.
        | OpScatter
        { -- | Binder for result value (a Unit)
          opResultBind          :: BindF

          -- | Bound of target vector.
        , opTargetVector        :: BoundF

          -- | Bound of source index series.
        , opSourceIndices       :: BoundF

          -- | Bound of source element series.
        , opSourceElems         :: BoundF

          -- | Rate of input serieses.
        , opInputRate           :: TypeF

          -- | Type of elements.
        , opElemType            :: TypeF
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
        , opResultSeries        :: BindF

          -- | Rate of all input series.
        , opInputRate           :: TypeF

          -- | Names for input series.
        , opInputSeriess        :: [BoundF]

          -- | Worker input parameters
        , opWorkerParams        :: [BindF]

          -- | Worker body
        , opWorkerBody          :: ExpF
        }

        -----------------------------------------
        -- | Fold all the elements of a series.
        | OpFold
        { -- | Binder for result value.
          opResultValue         :: BindF

          -- | Rate of input series.
        , opInputRate           :: TypeF

          -- | Bound of input series.
        , opInputSeries         :: BoundF

          -- | Starting accumulator value.
        , opZero                :: ExpF

          -- | Worker parameter for index input.
          -- Should be BNone for OpFlowFold, but something for OpFlowFoldIndex
        , opWorkerParamIndex    :: BindF

          -- | Worker parameter for accumulator input.
        , opWorkerParamAcc      :: BindF

          -- | Worker parameter for element input.
        , opWorkerParamElem     :: BindF

          -- | Worker body.
        , opWorkerBody          :: ExpF }

        -----------------------------------------
        -- | Pack a series according to a selector.
        | OpPack
        { -- | Binder for result series.
          opResultSeries        :: BindF

          -- | Rate of input series.
        , opInputRate           :: TypeF

          -- | Bound of input series.
        , opInputSeries         :: BoundF

          -- | Rate of output series.
        , opOutputRate          :: TypeF

          -- | Type of a series element.
        , opElemType            :: TypeF }
        deriving Show

