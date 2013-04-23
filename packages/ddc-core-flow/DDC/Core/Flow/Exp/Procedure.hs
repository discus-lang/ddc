
module DDC.Core.Flow.Exp.Procedure
        ( Procedure     (..)
        , Loop          (..)
        , Context       (..)
        , StmtStart     (..)
        , StmtBody      (..)
        , StmtEnd       (..)
        , Stmt          (..))
where
import DDC.Core.Exp
import DDC.Core.Flow.Prim


-- | An imperative procedure made up of some loops.
data Procedure
        = Procedure
        { procedureName         :: Name
        , procedureType         :: Type Name
        , procedureParamTypes   :: [Bind Name]
        , procedureParamValues  :: [Bind Name]
        , procedureNest         :: [Loop]  
        , procedureResult       :: Exp () Name }

-- | A loop nest.
data Loop
        = Loop
        { loopContext           :: Context
        , loopStart             :: [StmtStart]
        , loopBody              :: [StmtBody]
        , loopNested            :: [Loop]
        , loopEnd               :: [StmtEnd] 
        , loopResult            :: Exp () Name }
        deriving Show

data Context
        = Context (Type Name)
        deriving (Show, Eq)

-- | Statements that appear at the start of a loop.
--   These initialise accumulators.
data StmtStart
        -- Inititlise a new accumulator.
        = StartAcc 
        { startAccName          :: Name
        , startAccType          :: Type Name
        , startAccExp           :: Exp () Name }
        deriving Show


-- | Statements that appear in the body of a loop.
data StmtBody
        -- | Read from an accumulator.
        = BodyAccRead
        { -- | Name of the accumulator.
          bodyAccName           :: Name

          -- | Type of the accumulator.
        , bodyAccType           :: Type Name

          -- | Name of the read value
        , bodyAccNameBind       :: Bind Name
        }

        -- | Body of an accumulation operation.
        --   Writes to the accumulator.
        | BodyAccWrite

        { -- | Name of the accumulator.
          bodyAccName           :: Name

          -- | Type of the accumulator.
        , bodyAccType           :: Type Name

          -- | Stream being read.
        , bodyAccStream         :: Bound Name

          -- | Parameter that binds the next element from the
          --   stream in the body expression.
        , bodyAccParamElem      :: Bind Name

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
