
module DDC.Llvm.Exp
        ( Exp           (..)
        , CallType      (..))
where
import DDC.Llvm.Var
import DDC.Llvm.Prim
import DDC.Llvm.Type
import DDC.Llvm.Attr


-- Expression -------------------------------------------------------------------------------------
-- | LLVM Expressions.
data Exp
        -- Perform the machine operator op on the operands left and right.
        = XMachOp       MachOp Var Var

        -- Perform a compare operation on the operands left and right.
        | XCmpOp        CmpOp  Var Var

        -- | Allocate sizeof(tp) * amount bytes on the stack.
        | XAlloca       Type Int

        -- | Allocate sizeof(tp) * amount bytes on the heap.
        | XMalloc       Type Int

        -- | Load the value at location ptr.
        | XLoad         Var

        -- | Navigate in an structure, selecting elements.
        --      
        --    * flag:     Wwhether the computer computed pointer is inbounds.
        --
        --    * location: Location of the structure.
        --
        --    * indices:  A list of indices to select the final value.
        | XGetElemPtr   Bool Var [Var]

        -- | Cast the variable from to the to type. This is an abstraction of three
        --   cast operators in Llvm, inttoptr, prttoint and bitcast.
        --
        --    * cast: Cast type
        --
        --    * from: Variable to cast
        --
        --    * to:   type to cast to
        | XCast         CastOp Var Type

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
        | XCall         CallType Var [Var] [FuncAttr]

        -- | Merge variables from different basic blocks which are predecessors of this
        --   basic block in a new variable of type tp.
        --
        --    * tp:         type of the merged variable, must match the types of the
        --                  predecessor variables.
        --
        --    * precessors: A list of variables and the basic block that they originate
        --                  from.
        | XPhi          Type [(Var, Var)]

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
        | XAsm          String String Type [Var] Bool Bool
        deriving (Show, Eq)


-- CallType ---------------------------------------------------------------------------------------
-- | Different types to call a function.
data CallType
        -- | Normal call, allocate a new stack frame.
        = StdCall

        -- | Tail call, perform the call in the current stack frame.
        | TailCall
        deriving (Eq,Show)

