
module DDC.Llvm.Instr
        ( module DDC.Llvm.Exp
        , module DDC.Llvm.Type
        , module DDC.Llvm.Prim
        , module DDC.Llvm.Metadata
        , Label         (..)
        , Block         (..)
        , CallType      (..)
        , Instr         (..)
        
        -- * Instructions annotated with metadata
        , AnnotInstr    (..)
        , annotNil, annotWith )
where
import DDC.Llvm.Prim
import DDC.Llvm.Type
import DDC.Llvm.Exp
import DDC.Llvm.Metadata
import DDC.Base.Pretty
import Data.Sequence            (Seq)
import Data.List
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
        , blockStmts    :: Seq AnnotInstr
        }


instance Pretty Block where
 ppr (Block label instrs)
        =    ppr label <> colon
        <$$> indent 8 (vcat $ map ppr $ Seq.toList instrs)


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
data AnnotInstr = AnnotInstr (Instr, [MDecl])
                  deriving Show
                  
instance Pretty AnnotInstr where
 ppr (AnnotInstr (instr, [])) = ppr instr
 ppr (AnnotInstr (instr, mds))
  = let pprWithTag (MDecl ref Tbaa{}) = text "!tbaa"  <> space <> ppr ref
        pprWithTag (MDecl ref Debug)  = text "!debug" <> space <> ppr ref
    in  ppr  instr
        <>   comma <> (hcat $ replicate 4 space)
        <>   (hcat $ punctuate (comma <> space) (map pprWithTag mds))
                                        
                                        
annotNil :: Instr -> AnnotInstr
annotNil ins = AnnotInstr (ins, [])

annotWith :: Instr -> [MDecl] -> AnnotInstr
annotWith ins mds = AnnotInstr (ins, mds)

                    
-- | Instructions
data Instr
        -- | Comment meta-instruction.
        = IComment      [String]

        -- | Set meta instruction v1 = value.
        --   This isn't accepted by the real LLVM compiler.
        --   ISet instructions are erased by the 'Clean' transform.
        | ISet          Var     Exp

        -- | No operation.
        --   This isn't accepted by the real LLVM compiler.
        --   INop instructions are erased by the 'Clean' transform.
        | INop


        -- Phi nodes --------------------------------------
        | IPhi          Var     [(Exp, Label)]


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
        | IOp           Var     Op      Exp     Exp


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


        -- Other Operations -------------------------------
        -- | Integer comparison.
        | IICmp         Var     ICond   Exp     Exp

        -- | Floating-point comparison.
        | IFCmp         Var     FCond   Exp     Exp

        -- | Call a function. 
        --   Only NoReturn, NoUnwind and ReadNone attributes are valid.
        | ICall         (Maybe Var) CallType Type Name [Exp] [FuncAttr]
        deriving (Show, Eq)



instance Pretty Instr where
 ppr ii
  = let -- Pad binding occurrence of variable.
        padVar  var
         = fill 12 (ppr $ nameOfVar var)

    in  case ii of
        -- Meta-instructions -------------------------------
        IComment strs           
         -> vcat $ map (semi <+>) $ map text strs

        ISet dst val
         -> hsep [ fill 12 (ppr $ nameOfVar dst)
                 , equals
                 , ppr val ]

        INop 
         -> text "nop"

        -- Phi nodes --------------------------------------
        IPhi vDst expLabels
         -> padVar vDst
                <+> equals
                <+> text "phi"
                <+> ppr (typeOfVar vDst)
                <+> hcat
                     (intersperse (comma <> space)
                        [ brackets
                                (   pprPlainX xSrc
                                <>  comma
                                <+> text "%" <> ppr label)
                        | (xSrc, label)         <- expLabels ])

        -- Terminator Instructions ------------------------
        IReturn Nothing         
         -> text "ret void"

        IReturn (Just value)    
         -> text "ret" <+> ppr value

        IBranch label
         -> text "br label %"  <> ppr label

        IBranchIf cond labelTrue labelFalse
         -> hsep [ text "br"
                 , ppr cond,      comma
                 , ppr labelTrue, comma
                 , ppr labelFalse ]

        ISwitch x1 lDefault alts
         -> text "switch"
                <+> ppr x1 <> comma
                <+> text "label %" <> ppr lDefault
                <+> lbracket
                <+> (hsep [ ppr discrim 
                                <> comma
                                <> text "label %" <> ppr dest
                                | (discrim, dest) <- alts ])
                <+> rbracket

        IUnreachable
         -> text "unreachable"

        -- Memory Operations ------------------------------
        ILoad vDst x1
         -> padVar vDst
                <+> equals
                <+> text "load"
                <+> ppr x1

        IStore xDst xSrc
         -> text "store"
                <+> ppr xSrc  <> comma
                <+> ppr xDst

        -- Binary Operations ------------------------------
        IOp vDst op x1 x2
         -> padVar vDst
                <+> equals
                <+> ppr op      <+> ppr (typeOfExp x1)
                <+> pprPlainX x1 <> comma 
                <+> pprPlainX x2

        -- Conversion operations --------------------------
        IConv vDst conv xSrc
         -> padVar vDst
                <+> equals
                <+> ppr conv
                <+> ppr xSrc
                <+> text "to"
                <+> ppr (typeOfVar vDst)

        -- Other operations -------------------------------
        IICmp vDst icond x1 x2
         -> padVar vDst
                <+> equals
                <+> text "icmp"  <+> ppr icond  <+> ppr (typeOfExp x1)
                <+> pprPlainX x1 <> comma
                <+> pprPlainX x2

        IFCmp vDst fcond x1 x2
         -> padVar vDst
                <+> equals
                <+> text "fcmp"  <+> ppr fcond  <+> ppr (typeOfExp x1)
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

