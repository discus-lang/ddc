
module DDC.Llvm.Statement
        ( Unique
        , LlvmBlockId
        , LlvmStatement  (..))
where
import DDC.Llvm.Exp
import DDC.Llvm.Var


-- | Block labels
type LlvmBlockId 
        = Unique


-- | LLVM Statements
data LlvmStatement
        -- | Plain comment  
        = Comment       [String]

        -- A nop LLVM statement. Useful as its often more efficient to use this
        -- then to wrap LLvmStatement in a Just or [].
        | Nop

        -- | An instruction for the optimizer that the code following is not reachable        
        | Unreachable

        -- | Raise an expression to a statement (if don't want result or want to use
        --   Llvm unnamed values.
        | Expr          Exp

        -- | Assign an expression to an variable:
        | Assignment    Var Exp

        -- | Store variable value in pointer ptr. If value is of type t then ptr must
        -- be of type t*.
        | Store         Var Var

        -- | Set a label on this position.
        | MkLabel       LlvmBlockId

        -- | Always branch to the target label
        | Branch        Var

        -- | Branch to label targetTrue if cond is true otherwise to label targetFalse
        | BranchIf      Var     Var     Var

        -- | Mutliway branch.
        --   Contains the scrutinee, default label if there is no match, and a list
        --   of alternatives. If the scrutinee matches the first var then jump 
        --   to the label in the second.
        | Switch        Var     Var     [(Var, Var)]
         
        -- | Return a result.
        | Return        (Maybe Var)
        deriving (Show, Eq)



