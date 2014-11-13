
-- | A `Procedure` is an abstract imperative loop nest. 
--   The loops are represented as a separated loop anatomy, to make it
--   easy to incrementally build them from a data flow graph expressed
--   as a `Process`.
--
module DDC.Core.Flow.Procedure
        ( Procedure     (..)
        , Nest          (..)
        , Context       (..)
        , StmtStart     (..)
        , StmtBody      (..)
        , StmtEnd       (..))
where
import DDC.Core.Flow.Exp
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Context
import Data.Monoid


-- | An imperative procedure made up of some loops.
data Procedure
        = Procedure
        { procedureName         :: Name
        , procedureParamTypes   :: [BindF]
        , procedureParamValues  :: [BindF]
        , procedureNest         :: Nest }

-- | A loop nest.
data Nest
        = NestEmpty

        | NestList
        { nestList              :: [Nest]}

        -- Used to define the outer loop of a process.
        | NestLoop
        { nestRate              :: Type Name
        , nestStart             :: [StmtStart]
        , nestBody              :: [StmtBody]
        , nestInner             :: Nest
        , nestEnd               :: [StmtEnd] }

        -- Guarded context, 
        -- used when lowering pack-like operations.
        | NestGuard
        { nestOuterRate         :: Type Name
        , nestInnerRate         :: Type Name
        , nestFlags             :: Bound Name
        , nestBody              :: [StmtBody] 
        , nestInner             :: Nest }

        -- Segmented context,
        -- used when lowering segmented operations.
        | NestSegment
        { nestOuterRate         :: Type Name
        , nestInnerRate         :: Type Name
        , nestLength            :: Bound Name
        , nestBody              :: [StmtBody]
        , nestInner             :: Nest }
        deriving Show


instance Monoid Nest where
 mempty  = NestEmpty

 mappend n1 n2
  = case (n1, n2) of
        (NestEmpty,    _)               -> n2
        (_,            NestEmpty)       -> n1
        (NestList ns1, NestList ns2)    -> NestList (ns1 ++ ns2)
        (NestList ns1, _)               -> NestList (ns1 ++ [n2])
        (_,            NestList ns2)    -> NestList (n1 : ns2)
        (_,            _)               -> NestList [n1, n2]


-- | Statements that can appear at the start of a loop.
--   These initialise accumulators.
data StmtStart
        -- | Evaluate a pure expression
        = StartStmt
        { startResultBind       :: Bind Name
        , startExpression       :: Exp () Name }

        -- | Allocate a new vector.
        | StartVecNew
        { startVecNewName       :: Name
        , startVecNewElemType   :: Type Name
        , startVecNewRate       :: Type Name }

        -- | Inititlise a new accumulator.
        | StartAcc 
        { startAccName          :: Name
        , startAccType          :: Type Name
        , startAccExp           :: Exp () Name }
        deriving Show


-- | Statements that appear in the body of a loop.
data StmtBody
        -- | Evaluate a pure expression.
        = BodyStmt
        { -- | Bind for the result
          bodyResultBind        :: Bind Name

          -- | Expression to evaluate
        , bodyExpression        :: Exp () Name }


        -- | Write to a vector.
        | BodyVecWrite
        { -- | Name of the vector.
          bodyVecName           :: Name

          -- | Type of the element.
        , bodyVecWriteElemType  :: Type Name

          -- | Expression for the index to write to.
        , bodyVecWriteIx        :: Exp () Name

          -- | Expression for the value to write.
        , bodyVecWriteVal       :: Exp () Name
        }


        -- | Read from an accumulator.
        | BodyAccRead
        { -- | Name of the accumulator.
          bodyAccName           :: Name

          -- | Type of the accumulator.
        , bodyAccType           :: Type Name

          -- | Binder for the read value.
        , bodyAccNameBind       :: Bind Name
        }


        -- | Body of an accumulation operation.
        --   Writes to the accumulator.
        | BodyAccWrite
        { -- | Name of the accumulator.
          bodyAccName           :: Name

          -- | Type of the accumulator.
        , bodyAccType           :: Type Name

          -- | Expression to update the accumulator.
        , bodyAccExp            :: Exp () Name }
        deriving Show


-- | Statements that appear after a loop to cleanup.
data StmtEnd
        -- | Generic ending statements.
        = EndStmt
        { endBind               :: Bind Name
        , endExp                :: Exp () Name }

        -- | Read the result of an accumulator.
        | EndAcc
        { endName               :: Name
        , endType               :: Type Name
        , endAccName            :: Name }

        -- | Destructively truncate a vector to its final size.
        | EndVecTrunc
        { endVecName            :: Name
        , endVecType            :: Type Name
        , endVecRate            :: Type Name }
        deriving Show
