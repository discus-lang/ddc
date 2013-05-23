
module DDC.Core.Flow.Procedure
        ( Procedure     (..)
        , Nest          (..)
        , Context       (..)
        , StmtStart     (..)
        , StmtBody      (..)
        , StmtEnd       (..)
        , Stmt          (..))
where
import DDC.Core.Exp
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Context
import Data.Monoid

-- | An imperative procedure made up of some loops.
data Procedure
        = Procedure
        { procedureName         :: Name
        , procedureParamTypes   :: [Bind Name]
        , procedureParamValues  :: [Bind Name]
        , procedureNest         :: Nest
        , procedureStmts        :: [Lets () Name]
        , procedureResult       :: Exp () Name 
        , procedureResultType   :: Type Name }


-- | A loop nest.
data Nest
        = NestEmpty

        | NestList
        { nestList              :: [Nest]}

        | NestLoop
        { nestRate              :: Type Name
        , nestStart             :: [StmtStart]
        , nestBody              :: [StmtBody]
        , nestInner             :: Nest
        , nestEnd               :: [StmtEnd] 
        , nestResult            :: Exp () Name }

        | NestIf
        { nestOuterRate         :: Type Name
        , nestInnerRate         :: Type Name
        , nestFlags             :: Bound Name
        , nestBody              :: [StmtBody] }
        deriving Show


instance Monoid Nest where
 mempty  = NestEmpty

 mappend n1 n2
  = case (n1, n2) of
        (NestEmpty, _)                  -> n2
        (_, NestEmpty)                  -> n1
        (NestList ns1, NestList ns2)    -> NestList (ns1 ++ ns2)
        (NestList ns1, _)               -> NestList (ns1 ++ [n2])
        (_, NestList ns2)               -> NestList (n1 : ns2)
        (_, _)                          -> NestList [n1, n2]


-- | Statements that appear at the start of a loop.
--   These initialise accumulators.
data StmtStart
        -- Allocate a new vector.
        = StartVecNew
        { startVecNewName       :: Name
        , startVecNewElemType   :: Type Name
        , startVecNewRate       :: Type Name }

        -- Inititlise a new accumulator.
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
        = EndStmts
        { endStmts              :: [Stmt] }

        | EndAcc
        { endName               :: Name
        , endType               :: Type Name
        , endAccName            :: Name }
        deriving Show


-- | Generic statement.
data Stmt
        = Stmt (Bind Name) (Exp () Name)
        deriving Show
