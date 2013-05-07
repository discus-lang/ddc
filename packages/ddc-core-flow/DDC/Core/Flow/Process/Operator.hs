
module DDC.Core.Flow.Process.Operator
        ( Operator (..)
        , elemTypeOfOperator)
where
import DDC.Core.Exp
import DDC.Core.Flow.Prim


-- | An abstract series operator.
data Operator
        -----------------------------------------
        -- | Apply a function to corresponding elements in a series,
        --   producing a new series. This subsumes regular 'map' as well
        --   as 'zipWith' like functions where the input lengths are identical.
        = OpMap
        { -- | Arity of map, number of input streams.
          opArity               :: Int

          -- | Rate of source and result streams.
        , opRate                :: Type Name

          -- | Bound for input stream.
        , opInputs              :: [Bound Name]

          -- | Binder for result stream.
        , opResult              :: Bind Name

          -- | Type of input elements.
        , opTypeElems           :: [Type Name]

          -- | Type of result stream element.
        , opTypeResult          :: Type Name

          -- | Worker input parameters
        , opWorkerParams        :: [Bind Name]

          -- | Worker body
        , opWorkerBody          :: Exp () Name
        }

        -----------------------------------------
        -- | Fold all the elements of a series.
        | OpFold
        { opRate                :: Type   Name

          -- | Binder for result stream.
        , opResult              :: Bind   Name

          -- | Bound of input stream.
        , opStream              :: Bound  Name

          -- | Type of fold accumulator.
        , opTypeAcc             :: Type   Name

          -- | Type of stream element.
        , opTypeStream          :: Type   Name

          -- | Starting accumulator value.
        , opZero                :: Exp () Name

          -- | Worker accumulator input.
        , opWorkerParamAcc      :: Bind   Name

          -- | Worker series element input.
        , opWorkerParamElem     :: Bind   Name

          -- | Worker body.
        , opWorkerBody          :: Exp () Name }


-- | Get the type of stream element that an operator processes.
elemTypeOfOperator :: Operator -> Maybe (Type Name)
elemTypeOfOperator op
 = case op of
        OpMap{}                 -> Nothing              -- TODO: handle multiple source elem types.
        OpFold{}                -> Just $ opTypeStream op
