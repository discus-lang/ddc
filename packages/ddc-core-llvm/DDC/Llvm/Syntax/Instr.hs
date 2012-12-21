
module DDC.Llvm.Syntax.Instr
        ( -- * Blocks
          Block         (..)
        , Label         (..)

        -- * Annotated instructions
        , AnnotInstr    (..)
        , annotNil
        , annotWith

        -- * Instructions
        , Instr         (..)
        , branchTargetsOfInstr
        , defVarOfInstr
        , defVarsOfBlock)
where
import DDC.Llvm.Syntax.Exp
import DDC.Llvm.Syntax.Prim
import DDC.Llvm.Syntax.Attr
import DDC.Llvm.Syntax.Metadata
import DDC.Llvm.Syntax.Type
import Data.Maybe
import Data.Sequence            (Seq)
import Data.Set                 (Set)
import qualified Data.Set       as Set
import qualified Data.Foldable  as Seq


-- Block ----------------------------------------------------------------------
-- | Block labels.
data Label
        = Label String
        deriving (Eq, Ord, Show)


-- | A block of LLVM code with an optional annotation.
data Block
        = Block 
        { -- | The code label for this block
          blockLabel    :: Label

          -- | A list of LlvmStatement's representing the code for this block.
          --   This list must end with a control flow statement.
        , blockInstrs   :: Seq AnnotInstr
        }


-- Instructions ---------------------------------------------------------------
-- | Instructions annotated with metadata.
data AnnotInstr 
        = AnnotInstr 
        { annotInstr    :: Instr
        , annotMDecl    :: [MDecl] }
        deriving Show


-- | Construct an annotated instruction with no annotations.
annotNil :: Instr -> AnnotInstr
annotNil ins = AnnotInstr ins []


-- | Annotate an instruction with some metadata.
annotWith :: Instr -> [MDecl] -> AnnotInstr
annotWith ins mds = AnnotInstr ins mds


-------------------------------------------------------------------------------                    
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
        | ICall         (Maybe Var) CallType (Maybe CallConv)
                        Type Name [Exp] [FuncAttr]
        deriving (Show, Eq)


-- | If this instruction can branch to a label then return the possible targets.
branchTargetsOfInstr :: Instr -> Maybe (Set Label)
branchTargetsOfInstr instr
 = case instr of
        IBranch l               
         -> Just $ Set.singleton l

        IBranchIf _ l1 l2
         -> Just $ Set.fromList [l1, l2]

        ISwitch _ lDef ls       
         -> Just $ Set.fromList (lDef : map snd ls) 

        _ -> Nothing


-- | Get the LLVM variable that this instruction assigns to, 
--   or `Nothing` if there isn't one.
defVarOfInstr :: Instr -> Maybe Var
defVarOfInstr instr
 = case instr of
        IComment{}      -> Nothing
        ISet var _      -> Just var
        INop            -> Nothing
        IPhi var _      -> Just var
        IReturn{}       -> Nothing
        IBranch{}       -> Nothing
        IBranchIf{}     -> Nothing
        ISwitch{}       -> Nothing
        IUnreachable{}  -> Nothing
        IOp var _ _ _   -> Just var
        IConv var _ _   -> Just var
        ILoad var _     -> Just var
        IStore{}        -> Nothing
        IICmp var _ _ _ -> Just var
        IFCmp var _ _ _ -> Just var
        ICall mvar _ _ _ _ _ _ -> mvar


-- | Get the set of LLVM variables that this block defines.
defVarsOfBlock :: Block -> Set Var
defVarsOfBlock (Block _ instrs)
        = Set.fromList
        $ mapMaybe (defVarOfInstr . annotInstr)
        $ Seq.toList instrs

