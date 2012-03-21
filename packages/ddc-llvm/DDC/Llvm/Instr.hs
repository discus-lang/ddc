
module DDC.Llvm.Instr
        ( module DDC.Llvm.Prim
        , module DDC.Llvm.Type
        , module DDC.Llvm.Var
        , BlockId       (..)
        , Block         (..)
        , Instr         (..)
        , CallType      (..))
where
import DDC.Llvm.Prim
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
        , blockStmts    :: [Instr]
        }


instance Pretty Block where
 ppr (Block blockId instrs)
        =    ppr blockId <> colon
        <$$> indent 8 (vcat $ map ppr instrs)


-- Instr ----------------------------------------------------------------------
-- | Instructions
data Instr
        -- | Comment meta-instruction.
        = IComment      [String]


        -- Terminator Instructions ------------------------
        -- | Return a result.
        | IReturn       (Maybe Value)

        -- | Unconditional branch to the target label.
        | IBranch       Var

        -- | Conditional branch.
        | IBranchIf     Var     Var     Var

        -- | Mutliway branch.
        --   Contains the scrutinee, default label if there is no match, and a list
        --   of alternatives. If the scrutinee matches the first var then jump 
        --   to the label in the second.
        | ISwitch       Var     Var     [(Var, Var)]

        -- | This instruction is used to inform the optimizer that a particular
        --   portion of the code is not reachable.   
        | IUnreachable


        -- Binary Operations ------------------------------
        | IOp           Var     Op      Var     Var


        -- Conversion Operations --------------------------
        -- | Cast the variable from to the to type. This is an abstraction of three
        --   cast operators in Llvm, inttoptr, prttoint and bitcast.
        | IConv         Var     Conv    Var     Type


        -- Memory Access and Addressing -------------------
        -- | Load a value from memory.
        | ILoad         Var     Ptr

        -- | Store a value to memory.
        | IStore        Value   Ptr

        -- | Navigate in an structure, selecting elements.
        --      
        --   * flag:     Whether the computer computed pointer is inbounds.
        --
        --   * location: Location of the structure.
        --
        --   * indices:  A list of indices to select the final value.
        | XGetElemPtr   Var     Bool    Var     [Var]


        -- Other Operations -------------------------------
        -- | Integer comparison.
        | IICmp         Var     ICond   Var     Var

        -- | Floating-point comparison.
        | IFCmp         Var     FCond   Var     Var


        -- | Call a function. 
        --
        --   * tailJumps: CallType to signal if the function should be tail called.
        --
        --   * fnptrval:  An LLVM value containing a pointer to a function to be invoked.
        --
        --   * args:      Arguments for the parameters.
        --
        --   * attrs:     A list of function attributes for the call. 
        --                Only NoReturn, NoUnwind, ReadOnly and ReadNone are valid here.
        | ICall         Var     CallType Ptr [Var] [FuncAttr]
        deriving (Show, Eq)


-- | Different types to call a function.
data CallType
        -- | Normal call, allocate a new stack frame.
        = StdCall

        -- | Tail call, perform the call in the current stack frame.
        | TailCall
        deriving (Eq,Show)


instance Pretty Instr where
 ppr ii
  = case ii of
        -- | Comment meta-instruction
        IComment strs           -> vcat $ map (semi <+>) $ map text strs

        -- Terminator Instructions ------------------------
        IReturn Nothing         
         -> text "ret void"

        IReturn (Just value)    
         -> text "ret" <+> ppr (typeOfVar value) <+> ppr value

        IBranch label
         -> text "br label" <+> ppr label

        IBranchIf cond labelTrue labelFalse
         -> hsep [ text "br"
                 , text "i1",    ppr cond,      comma
                 , text "label", ppr labelTrue, comma
                 , text "label", ppr labelFalse ]

        IUnreachable
         -> text "unreachable"

        -- Binary Operations ------------------------------
        IOp dst op v1 v2
         -> hcat [ fill 12 (ppr dst)
                 , equals
                 , ppr op, ppr v1, comma, ppr v2]

        _ -> text "INSTR" <> text (show ii)


