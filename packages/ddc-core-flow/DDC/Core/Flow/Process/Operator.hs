
module DDC.Core.Flow.Process.Operator
        ( Operator (..)
        , resultTypeOfOperator)
where
import DDC.Core.Exp
import DDC.Core.Flow.Prim
import DDC.Core.Compounds


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

          -- | Binder for result stream.
        , opResult              :: Bind Name

          -- | Bound for input stream.
        , opInputs              :: [Bound Name]


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
        { 
          -- | Rate of source stream.
          opRate                :: Type   Name

          -- | Binder for result stream.
        , opResult              :: Bind   Name

          -- | Bound of input stream.
        , opInput               :: Bound  Name

          -- | Type of fold accumulator.
        , opTypeAcc             :: Type   Name

          -- | Type of stream element.
        , opTypeElem            :: Type   Name

          -- | Starting accumulator value.
        , opZero                :: Exp () Name

          -- | Worker accumulator input.
        , opWorkerParamAcc      :: Bind   Name

          -- | Worker series element input.
        , opWorkerParamElem     :: Bind   Name

          -- | Worker body.
        , opWorkerBody          :: Exp () Name }

        deriving (Show)


-- | Get the type of stream element that an operator processes.
resultTypeOfOperator :: Operator -> Type Name
resultTypeOfOperator op
        = typeOfBind $ opResult op
