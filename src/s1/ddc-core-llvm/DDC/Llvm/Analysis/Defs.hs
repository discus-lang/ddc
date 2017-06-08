
module DDC.Llvm.Analysis.Defs
        ( Def (..)
        , takeDefOfInstr
        , defsOfBlock)
where
import DDC.Llvm.Syntax
import Data.Map                         (Map)
import qualified Data.Foldable          as Seq
import qualified Data.Map               as Map


-- | How a variable is defined.
data Def
        -- | Variable is given a non-constant value.
        = DefVar

        -- | Variable is an alias of another variable.
        | DefAlias              Var

        -- | Variable binds some closed, constant expression.
        | DefClosedConstant     Exp
        deriving Show


-- | Collect information about how all the local variables in this block
--   are defined.
defsOfBlock :: Block -> Map Var (Label, Def)
defsOfBlock block
        = Map.fromList
        $ [ (v, (blockLabel block, def))
                | Just (v, def) <- map (takeDefOfInstr . annotInstr)
                                $  Seq.toList $ blockInstrs block ]


-- | If this instruction defines a variable,
--   then collect some information about it.
takeDefOfInstr :: Instr -> Maybe (Var, Def)
takeDefOfInstr instr
 = case instr of
        -- Comments
        IComment{}     
         -> Nothing

        -- Set meta instruction.
        ISet v1 x2
         | XVar v2      <- x2
         -> Just (v1, DefAlias v2)

         | isClosedConstantExp x2
         -> Just (v1, DefClosedConstant x2)

         | otherwise            
         -> Just (v1, DefVar)

        -- No operation.
        INop            -> Nothing

        -- Phi nodes
        -- Even if both branches are constant, 
        -- we can't form an expression to represent this,
        -- so the result gets marked as non-constant.
        IPhi v _        -> Just (v, DefVar)

        -- Terminator Instructions
        IReturn{}       -> Nothing
        IBranch{}       -> Nothing
        IBranchIf{}     -> Nothing
        ISwitch{}       -> Nothing
        IUnreachable{}  -> Nothing

        -- Binary Operators
        IOp v _ _ _     -> Just (v, DefVar)

        -- Conversion Operators
        IConv v _ _     -> Just (v, DefVar)

        -- Get element pointer
        IGet  v _ _     -> Just (v, DefVar)

        -- Allocate a value on the stack.
        IAlloca v _     -> Just (v, DefVar)

        -- Load a value from memory.
        ILoad v _       -> Just (v, DefVar)

        -- Store a value to memory.
        IStore{}        -> Nothing

        -- Comparisons
        ICmp v _ _ _    -> Just (v, DefVar)

        -- Function calls
        ICall mv _ _ _ _ _ _
         -> case mv of
                Just v  -> Just (v, DefVar)
                _       -> Nothing



