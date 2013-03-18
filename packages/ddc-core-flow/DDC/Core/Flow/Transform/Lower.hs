
module DDC.Core.Flow.Transform.Lower
        ( Context       (..)
        , LoopRate      (..)
        , Loop          (..)
        , StmtStart     (..)
        , StmtBody      (..)
        , Stmt)
where
import DDC.Core.Exp
import DDC.Core.Flow.Name


-- | Loop contexts.
data Context
        = ContextTop
        | ContextRate   LoopRate
        deriving Show


-- | Type expression defining the rate this loop is at.
data LoopRate
        = LoopRate (Type Name)
        deriving Show

-- | A stream processing loop.
data Loop
        = Loop
        { loopContext           :: Context
        , loopStart             :: [StmtStart]
        , loopBody              :: [StmtBody]
        , loopEnd               :: [Stmt] }
        deriving Show


-- | Statements that appear at the start of a loop.
--   These initialise accumulators.
data StmtStart
        = StartAcc (Bind Name) (Exp () Name)
        deriving Show

-- | Statements that appear in the body of a loop.
data StmtBody
        = BodyStmt
        { bodyStmt              :: [Stmt] }

        | BodyLoop
        { bodyLoop              :: Loop }
        deriving Show


data Stmt
        = Stmt (Bind Name) (Exp () Name)
        deriving Show
