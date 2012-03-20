
module DDC.Llvm.Stmt
        ( BlockId       (..)
        , Block         (..)
        , Stmt          (..))
where
import DDC.Llvm.Exp
import DDC.Llvm.Type
import DDC.Llvm.Var
import DDC.Base.Pretty


-- BlockId --------------------------------------------------------------------
-- | Block labels
data BlockId 
        = BlockId Unique
        deriving (Eq, Show)

instance Pretty BlockId where
 ppr (BlockId i)
        = text "block" <> int i


-- Block ----------------------------------------------------------------------
-- | A block of LLVM code.
data Block 
        = Block 
        { -- | The code label for this block
          blockId       :: BlockId

          -- | A list of LlvmStatement's representing the code for this block.
          -- This list must end with a control flow statement.
        , blockStmts    :: [Stmt]
        }


instance Pretty Block where
 ppr (Block blockId stmts)
        =    ppr (SMkLabel blockId)
        <$$> indent 8 (vcat $ map ppr stmts)


-- Stmt -----------------------------------------------------------------------
-- | LLVM Statements
data Stmt
        -- | Plain comment  
        = SComment       [String]

        -- A nop LLVM statement. Useful as its often more efficient to use this
        -- then to wrap LLvmStatement in a Just or [].
        | SNop

        -- | Indicates that the following code is unreachable.        
        | SUnreachable

        -- | Raise an expression to a statement 
        --   (if don't want result or want to use Llvm unnamed values.
        | SExp          Exp

        -- | Assign an expression to a variable.
        | SAssign       Var Exp

        -- | Store variable value in pointer ptr. 
        --  If value is of type t then ptr must be of type t*.
        | SStore        Var Var

        -- | Set a label on this position.
        | SMkLabel      BlockId

        -- | Always branch to the target label.
        | SBranch       Var

        -- | Branch to label targetTrue if cond is true otherwise to label targetFalse.
        | SBranchIf     Var     Var     Var

        -- | Mutliway branch.
        --   Contains the scrutinee, default label if there is no match, and a list
        --   of alternatives. If the scrutinee matches the first var then jump 
        --   to the label in the second.
        | SSwitch       Var     Var     [(Var, Var)]
         
        -- | Return a result.
        | SReturn       (Maybe Var)
        deriving (Show, Eq)


instance Pretty Stmt where
 ppr ss
  = case ss of
        SNop                    -> empty
        SUnreachable            -> text "unreachable"

        SMkLabel blockId
         -> ppr blockId <> colon

        SReturn (Just var)
         -> text "ret" <+> ppr var

        SReturn Nothing
         -> text "ret" <+> ppr TVoid

        -----------------------------------------

        _                       -> text "STMT" <> text (show ss)


