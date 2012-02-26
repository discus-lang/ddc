
module DDC.Llvm.Statement
        ( LlvmStatement  (..)
        , LlvmExpression
        , LlvmCallType   (..))
where
import DDC.Llvm.Var
import DDC.Llvm.Prim
import DDC.Llvm.Type
import DDC.Llvm.Attr
import DDC.Llvm.Base


-- | Llvm Statements
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
        --
        --  * dest:   Variable to assign to
        --  * source: Source expression
        | Assignment    LlvmVar LlvmExpression

        -- | Store variable value in pointer ptr. If value is of type t then ptr must
        -- be of type t*.
        --   * value: Variable/Constant to store.
        --   * ptr:   Location to store the value in        
        | Store         LlvmVar LlvmVar

        -- | Set a label on this position.
        --   * name: Identifier of this label, unique for this module
        | MkLabel       LlvmBlockId

        -- | Always branch to the target label
        | Branch        LlvmVar

        -- | Branch to label targetTrue if cond is true otherwise to label targetFalse
        --   
        --   * cond:        condition that will be tested, must be of type i1
        --   * targetTrue:  label to branch to if cond is true
        --   * targetFalse: label to branch to if cond is false
        | BranchIf      LlvmVar LlvmVar LlvmVar

        -- | Mutliway branch
        --   * scrutinee: Variable or constant which must be of integer type that is
        --                determines which arm is chosen.
        --   * def:       The default label if there is no match in target.
        --   * target:    A list of (value,label) where the value is an integer
        --                constant and label the corresponding label to jump to if the
        --                scrutinee matches the value.
        | Switch LlvmVar LlvmVar [(LlvmVar, LlvmVar)]
         
        -- | Return a result.
        --  * result: The variable or constant to return
        | Return (Maybe LlvmVar)
        deriving (Show, Eq)


-- Expression -------------------------------------------------------------------------------------
-- | Llvm Expressions
data LlvmExpression

        -- Perform the machine operator op on the operands left and right
        --  * op:    operator
        --  * left:  left operand
        --  * right: right operand
        = LlvmOp        LlvmMachOp LlvmVar LlvmVar

        -- Perform a compare operation on the operands left and right
        --  * op:    operator
        --  * left:  left operand
        --  * right: right operand        
        | Compare       LlvmCmpOp LlvmVar LlvmVar

        -- | Allocate amount * sizeof(tp) bytes on the stack
        --  * tp:     LlvmType to reserve room for
        --  * amount: The nr of tp's which must be allocated
        | Alloca        LlvmType Int

        -- | Allocate amount * sizeof(tp) bytes on the heap
        --   * tp:     LlvmType to reserve room for
        --   * amount: The nr of tp's which must be allocated        
        | Malloc        LlvmType Int


        -- | Load the value at location ptr
        | Load LlvmVar

        -- | Navigate in an structure, selecting elements
        --   * inbound: Is the pointer inbounds? (computed pointer doesn't overflow)
        --   * ptr:     Location of the structure
        --   * indexes: A list of indexes to select the correct value.        
        | GetElemPtr    Bool LlvmVar [LlvmVar]

        -- | Cast the variable from to the to type. This is an abstraction of three
        --   cast operators in Llvm, inttoptr, prttoint and bitcast.
        --    * cast: Cast type
        --    * from: Variable to cast
        --    * to:   type to cast to
        | Cast  LlvmCastOp LlvmVar LlvmType

        -- | Call a function. The result is the value of the expression.
        --  * tailJumps: CallType to signal if the function should be tail called
        --  * fnptrval:  An LLVM value containing a pointer to a function to be
        --               invoked. Can be indirect. Should be LMFunction type.
        --  * args:      Concrete arguments for the parameters
        --  * attrs:     A list of function attributes for the call. Only NoReturn,
        --               NoUnwind, ReadOnly and ReadNone are valid here.
        | Call  LlvmCallType LlvmVar [LlvmVar] [LlvmFuncAttr]

        -- | Merge variables from different basic blocks which are predecessors of this
        --   basic block in a new variable of type tp.
        --  * tp:         type of the merged variable, must match the types of the
        --              predecessor variables.
        --  * precessors: A list of variables and the basic block that they originate
        --                from.
        | Phi   LlvmType [(LlvmVar,LlvmVar)]

        -- | Inline assembly expression. Syntax is very similar to the style used by GCC.
        --   * assembly:   Actual inline assembly code.
        --   * contraints: Operand constraints.
        --   * return ty:  Return type of function.
        --   * vars:       Any variables involved in the assembly code.
        --   * sideeffect: Does the expression have side effects not visible from the
        --                constraints list.
        --   * alignstack: Should the stack be conservatively aligned before this
        --                 expression is executed.
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

