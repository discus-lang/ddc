
module DDC.Core.Blue.Prim.Base
        ( Name          (..)
        , TyConPrim     (..)
        , OpPrimArith   (..))
where
import Data.Typeable


-- | Names of things used in Disciple Core Blue.
data Name
        -- | User defined variables.
        = NameVar               String

        -- | A user defined constructor.
        | NameCon               String

        -- Machine primitives ------------------
        -- | A primitive type constructor.
        | NameTyConPrim         TyConPrim

        -- | Primitive arithmetic, logic, comparison and bit-wise operators.
        | NameOpPrimArith       OpPrimArith

        -- Literals -----------------------------
        -- | A boolean literal.
        | NameLitBool           Bool

        -- | A natural literal.
        | NameLitNat            Integer

        -- | An integer literal.
        | NameLitInt            Integer

        -- | A word literal.
        | NameLitWord           Integer Int
        deriving (Eq, Ord, Show, Typeable)


-- TyConPrim ------------------------------------------------------------------
-- | Primitive type constructors.
data TyConPrim
        -- | @Bool@ unboxed booleans.
        = TyConPrimBool

        -- | @Nat@ natural numbers.
        --   Big enough to count every addressable byte in the store.
        | TyConPrimNat

        -- | @Int@ signed integers.
        | TyConPrimInt

        -- | @WordN@ machine words of the given width.
        | TyConPrimWord   Int
        deriving (Eq, Ord, Show)


-- OpPrimArith ----------------------------------------------------------------
-- | Primitive arithmetic, logic, and comparison opretors.
--   We expect the backend/machine to be able to implement these directly.
--
--   For the Shift Right operator, the type that it is used at determines
--   whether it is an arithmetic (with sign-extension) or logical
--   (no sign-extension) shift.
data OpPrimArith
        -- numeric
        = OpPrimArithNeg  -- ^ Negation
        | OpPrimArithAdd  -- ^ Addition
        | OpPrimArithSub  -- ^ Subtraction
        | OpPrimArithMul  -- ^ Multiplication
        | OpPrimArithDiv  -- ^ Division
        | OpPrimArithMod  -- ^ Modulus
        | OpPrimArithRem  -- ^ Remainder

        -- comparison
        | OpPrimArithEq   -- ^ Equality
        | OpPrimArithNeq  -- ^ Negated Equality
        | OpPrimArithGt   -- ^ Greater Than
        | OpPrimArithGe   -- ^ Greater Than or Equal
        | OpPrimArithLt   -- ^ Less Than
        | OpPrimArithLe   -- ^ Less Than or Equal

        -- boolean
        | OpPrimArithAnd  -- ^ Boolean And
        | OpPrimArithOr   -- ^ Boolean Or

        -- bitwise
        | OpPrimArithShl  -- ^ Shift Left
        | OpPrimArithShr  -- ^ Shift Right
        | OpPrimArithBAnd -- ^ Bit-wise And
        | OpPrimArithBOr  -- ^ Bit-wise Or
        | OpPrimArithBXOr -- ^ Bit-wise eXclusive Or
        deriving (Eq, Ord, Show)

