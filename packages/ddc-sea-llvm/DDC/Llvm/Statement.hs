
module DDC.Llvm.Statement
        ( Unique
        , LlvmBlockId
        , LlvmStatement  (..)
        , LlvmExpression (..)
        , LlvmCallType   (..))
where
import DDC.Llvm.Var
import DDC.Llvm.Prim
import DDC.Llvm.Type
import DDC.Llvm.Attr

-- | Block labels
type LlvmBlockId 
        = Unique



-- | LLVM Statements
data LlvmStatement
        -- | Plain comment  
        = Comment       [LMString]

        -- A nop LLVM statement. Useful as its often more efficient to use this
        -- then to wrap LLvmStatement in a Just or [].
        | Nop

        -- | An instruction for the optimizer that the code following is not reachable        
        | Unreachable

        -- | Raise an expression to a statement (if don't want result or want to use
        --   Llvm unnamed values.
        | Expr LlvmExpression

        -- | Assign an expression to an variable:
        | Assignment    LlvmVar LlvmExpression

        -- | Store variable value in pointer ptr. If value is of type t then ptr must
        -- be of type t*.
        | Store         LlvmVar LlvmVar

        -- | Set a label on this position.
        | MkLabel       LlvmBlockId

        -- | Always branch to the target label
        | Branch        LlvmVar

        -- | Branch to label targetTrue if cond is true otherwise to label targetFalse
        | BranchIf      LlvmVar LlvmVar LlvmVar

        -- | Mutliway branch.
        --   Contains the scrutinee, default label if there is no match, and a list
        --   of alternatives. If the scrutinee matches the first var then jump 
        --   to the label in the second.
        | Switch LlvmVar LlvmVar [(LlvmVar, LlvmVar)]
         
        -- | Return a result.
        | Return (Maybe LlvmVar)
        deriving (Show, Eq)


-- Expression -------------------------------------------------------------------------------------
-- | LLVM Expressions.
data LlvmExpression

        -- Perform the machine operator op on the operands left and right.
        = LlvmOp        LlvmMachOp LlvmVar LlvmVar

        -- Perform a compare operation on the operands left and right.
        | Compare       LlvmCmpOp LlvmVar LlvmVar

        -- | Allocate sizeof(tp) * amount bytes on the stack.
        | Alloca        LlvmType Int

        -- | Allocate sizeof(tp) * amount bytes on the heap.
        | Malloc        LlvmType Int

        -- | Load the value at location ptr.
        | Load LlvmVar

        -- | Navigate in an structure, selecting elements.
        --      
        --    * flag:     Wwhether the computer computed pointer is inbounds.
        --
        --    * location: Location of the structure.
        --
        --    * indices:  A list of indices to select the final value.
        | GetElemPtr    Bool LlvmVar [LlvmVar]

        -- | Cast the variable from to the to type. This is an abstraction of three
        --   cast operators in Llvm, inttoptr, prttoint and bitcast.
        --
        --    * cast: Cast type
        --
        --    * from: Variable to cast
        --
        --    * to:   type to cast to
        | Cast  LlvmCastOp LlvmVar LlvmType

        -- | Call a function. The result is the value of the expression.
        --
        --    * tailJumps: CallType to signal if the function should be tail called
        --
        --    * fnptrval:  An LLVM value containing a pointer to a function to be
        --                 invoked. Can be indirect. Should be LMFunction type.
        --
        --    * args:      Concrete arguments for the parameters
        --
        --    * attrs:     A list of function attributes for the call. Only NoReturn,
        --                 NoUnwind, ReadOnly and ReadNone are valid here.
        | Call  LlvmCallType LlvmVar [LlvmVar] [LlvmFuncAttr]

        -- | Merge variables from different basic blocks which are predecessors of this
        --   basic block in a new variable of type tp.
        --
        --    * tp:         type of the merged variable, must match the types of the
        --                  predecessor variables.
        --
        --    * precessors: A list of variables and the basic block that they originate
        --                  from.
        | Phi   LlvmType [(LlvmVar,LlvmVar)]

        -- | Inline assembly expression. Syntax is very similar to the style used by GCC.
        --
        --    * assembly:   Actual inline assembly code.
        --
        --    * contraints: Operand constraints.
        --
        --    * return ty:  Return type of function.
        --
        --    * vars:       Any variables involved in the assembly code.
        --
        --    * sideeffect: Does the expression have side effects not visible from the
        --                 constraints list.
        --
        --    * alignstack: Should the stack be conservatively aligned before this
        --                  expression is executed.
        | Asm   LMString LMString LlvmType [LlvmVar] Bool Bool
        deriving (Show, Eq)


-- CallType ---------------------------------------------------------------------------------------
-- | Different types to call a function.
data LlvmCallType
  -- | Normal call, allocate a new stack frame.
  = StdCall

  -- | Tail call, perform the call in the current stack frame.
  | TailCall
  deriving (Eq,Show)

