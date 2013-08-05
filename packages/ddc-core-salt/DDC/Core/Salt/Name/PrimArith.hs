
module DDC.Core.Salt.Name.PrimArith
        ( PrimArith (..)
        , readPrimArith)
where
import DDC.Base.Pretty
import Control.DeepSeq
import Data.List


-- | Primitive arithmetic, logic, and comparison opretors.
--   We expect the backend/machine to be able to implement these directly.
--
--   For the Shift Right operator, the type that it is used at determines
--   whether it is an arithmetic (with sign-extension) or logical
--   (no sign-extension) shift.
data PrimArith
        -- numeric
        = PrimArithNeg  -- ^ Negation
        | PrimArithAdd  -- ^ Addition
        | PrimArithSub  -- ^ Subtraction
        | PrimArithMul  -- ^ Multiplication
        | PrimArithDiv  -- ^ Division
        | PrimArithMod  -- ^ Modulus
        | PrimArithRem  -- ^ Remainder

        -- comparison
        | PrimArithEq   -- ^ Equality
        | PrimArithNeq  -- ^ Negated Equality
        | PrimArithGt   -- ^ Greater Than
        | PrimArithGe   -- ^ Greater Than or Equal
        | PrimArithLt   -- ^ Less Than
        | PrimArithLe   -- ^ Less Than or Equal

        -- boolean
        | PrimArithAnd  -- ^ Boolean And
        | PrimArithOr   -- ^ Boolean Or

        -- bitwise
        | PrimArithShl  -- ^ Shift Left
        | PrimArithShr  -- ^ Shift Right
        | PrimArithBAnd -- ^ Bit-wise And
        | PrimArithBOr  -- ^ Bit-wise Or
        | PrimArithBXOr -- ^ Bit-wise eXclusive Or
        deriving (Eq, Ord, Show)

instance NFData PrimArith

instance Pretty PrimArith where
 ppr op
  = let Just (_, n) = find (\(p, _) -> op == p) primArithNames
    in  (text n)


-- | Read a primitive operator.
readPrimArith :: String -> Maybe PrimArith
readPrimArith str
  =  case find (\(_, n) -> str == n) primArithNames of
        Just (p, _)     -> Just p
        _               -> Nothing


-- | Names of primitve operators.
primArithNames :: [(PrimArith, String)]
primArithNames
 =      [ (PrimArithNeg,        "neg#")
        , (PrimArithAdd,        "add#")
        , (PrimArithSub,        "sub#")
        , (PrimArithMul,        "mul#")
        , (PrimArithDiv,        "div#")
        , (PrimArithRem,        "rem#")
        , (PrimArithMod,        "mod#")
        , (PrimArithEq ,        "eq#" )
        , (PrimArithNeq,        "neq#")
        , (PrimArithGt ,        "gt#" )
        , (PrimArithGe ,        "ge#" )
        , (PrimArithLt ,        "lt#" )
        , (PrimArithLe ,        "le#" )
        , (PrimArithAnd,        "and#")
        , (PrimArithOr ,        "or#" ) 
        , (PrimArithShl,        "shl#")
        , (PrimArithShr,        "shr#")
        , (PrimArithBAnd,       "band#")
        , (PrimArithBOr,        "bor#")
        , (PrimArithBXOr,       "bxor#") ]

