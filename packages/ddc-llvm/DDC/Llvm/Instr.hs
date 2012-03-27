
module DDC.Llvm.Instr
        ( module DDC.Llvm.Exp
        , module DDC.Llvm.Type
        , module DDC.Llvm.Prim
        , Label         (..)
        , Block         (..)
        , Index         (..)
        , CallType      (..)
        , Instr         (..))
where
import DDC.Llvm.Prim
import DDC.Llvm.Type
import DDC.Llvm.Exp
import DDC.Base.Pretty
import Data.Sequence            (Seq)
import qualified Data.Foldable  as Seq


-- Label ----------------------------------------------------------------------
-- | Block labels.
data Label
        = Label String
        deriving (Eq, Show)

instance Pretty Label where
 ppr (Label str)        = text str


-- Block ----------------------------------------------------------------------
-- | A block of LLVM code.
data Block 
        = Block 
        { -- | The code label for this block
          blockLabel    :: Label

          -- | A list of LlvmStatement's representing the code for this block.
          -- This list must end with a control flow statement.
        , blockStmts    :: Seq Instr
        }


instance Pretty Block where
 ppr (Block label instrs)
        =    ppr label <> colon
        <$$> indent 8 (vcat $ map ppr $ Seq.toList instrs)


-- Index ----------------------------------------------------------------------
-- | Index for the GetElemPtr instruction
data Index
        = Index 
        { indexBits     :: Int          -- ^ Width of index value.
        , indexValue    :: Integer  }   -- ^ Index value.
        deriving (Eq, Show)


instance Pretty Index where
 ppr (Index bits value)
        = text "i" <> int bits <+> integer value


-- CallType -------------------------------------------------------------------
-- | Different types to call a function.
data CallType
        -- | Normal call, allocate a new stack frame.
        = CallTypeStd

        -- | Tail call, perform the call in the current stack frame.
        | CallTypeTail
        deriving (Eq,Show)


instance Pretty CallType where
 ppr ct
  = case ct of
        CallTypeStd     -> empty
        CallTypeTail    -> text "tail"


-- Instr ----------------------------------------------------------------------
-- | Instructions
data Instr
        -- | Comment meta-instruction.
        = IComment      [String]

        -- | Set meta instruction v1 = value.
        | ISet          Var     Exp

        -- Terminator Instructions ------------------------
        -- | Return a result.
        | IReturn       (Maybe Exp)

        -- | Unconditional branch to the target label.
        | IBranch       Label

        -- | Conditional branch.
        | IBranchIf     Exp     Label   Label

        -- | Mutliway branch.
        --   If scruitniee matches one of the literals in the list then jump
        --   to the corresponding label, otherwise jump to the default.
        | ISwitch       Exp     Label   [(Lit, Label)]

        -- | Informs the optimizer that instructions after this point are unreachable.
        | IUnreachable


        -- Binary Operations ------------------------------
        | IOp           Var     Op      Type    Exp     Exp


        -- Conversion Operations --------------------------
        -- | Cast the variable from to the to type. This is an abstraction of three
        --   cast operators in Llvm, inttoptr, prttoint and bitcast.
        | IConv         Var     Conv    Exp


        -- Memory Access and Addressing -------------------
        -- | Load a value from memory.
        | ILoad         Var     Exp

        -- | Store a value to memory.
        --   First expression gives the destination pointer.
        | IStore        Exp     Exp

        -- | Navigate in an structure, selecting elements.
        --      
        --   * flag:     Whether the computer computed pointer is inbounds.
        --
        --   * location: Location of the structure.
        --
        --   * indices:  A list of indices to select the final value.
        | XGetElemPtr   Var     Bool    Exp     [Index]


        -- Other Operations -------------------------------
        -- | Integer comparison.
        | IICmp         Var     ICond   Type    Exp     Exp

        -- | Floating-point comparison.
        | IFCmp         Var     FCond   Type    Exp     Exp


        -- | Call a function. 
        --   Only NoReturn, NoUnwind and ReadNone attributes are valid.
        | ICall         (Maybe Var) CallType Type Name [Exp] [FuncAttr]
        deriving (Show, Eq)




instance Pretty Instr where
 ppr ii
  = case ii of
        -- Meta-instructions -------------------------------
        IComment strs           
         -> vcat $ map (semi <+>) $ map text strs

        ISet dst val
         -> hsep [ fill 12 (ppr $ nameOfVar dst)
                 , equals
                 , ppr val ]

        -- Terminator Instructions ------------------------
        IReturn Nothing         
         -> text "ret void"

        IReturn (Just value)    
         -> text "ret" <+> ppr value

        IBranch label
         -> text "br"  <+> ppr label

        IBranchIf cond labelTrue labelFalse
         -> hsep [ text "br"
                 , ppr cond,      comma
                 , ppr labelTrue, comma
                 , ppr labelFalse ]

        ISwitch x1 lDefault alts
         -> text "switch"
                <+> ppr x1 <> comma
                <+> text "label" <+> ppr lDefault
                <+> (hsep [ ppr discrim 
                                <> comma
                                <> text "label" <+> ppr dest
                                | (discrim, dest) <- alts ])

        IUnreachable
         -> text "unreachable"

        -- Binary Operations ------------------------------
        IOp dst op t x1 x2
         -> (fill 12 (ppr $ nameOfVar dst))
                <+> equals
                <+> ppr op      <+> ppr t
                <+> pprPlainX x1 <> comma 
                <+> pprPlainX x2


        -- Other operations -------------------------------
        IICmp dst icond t x1 x2
         -> (fill 12 (ppr $ nameOfVar dst))
                <+> equals
                <+> text "icmp"  <+> ppr icond  <+> ppr t
                <+> pprPlainX x1 <> comma
                <+> pprPlainX x2

        IFCmp dst fcond t x1 x2
         -> (fill 12 (ppr $ nameOfVar dst))
                <+> equals
                <+> text "fcmp"  <+> ppr fcond  <+> ppr t
                <+> pprPlainX x1 <> comma
                <+> pprPlainX x2

        ICall mdst callType tResult name xsArgs attrs
         -> let call'
                 = case callType of
                        CallTypeTail    -> text "tail call"
                        _               -> text "call"
                dst'
                 = case mdst of
                        Nothing         -> empty
                        Just dst        -> fill 12 (ppr $ nameOfVar dst) <+> equals <> space

            in dst' 
                <> hsep  [ call'
                         , ppr tResult
                         , ppr name
                         , encloseSep lparen rparen (comma <> space) (map ppr xsArgs)
                         , hsep $ map ppr attrs ]

        _ -> text "INSTR" <> text (show ii)


