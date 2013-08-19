
module DDC.Core.Tetra.Prim.Base
        ( Name          (..)
        , TyConPrim     (..)
        , PrimArith     (..)
        , PrimRef       (..))
where
import Data.Typeable
import DDC.Core.Salt.Name.PrimArith


-- | Names of things used in Disciple Core Tetra.
data Name
        -- | User defined variables.
        = NameVar               String

        -- | A user defined constructor.
        | NameCon               String

        -- Machine primitives ------------------
        -- | A primitive type constructor.
        | NameTyConPrim         TyConPrim

        -- | Primitive arithmetic, logic, comparison and bit-wise operators.
        | NamePrimArith         PrimArith

        -- | Mutable references.
        | NamePrimRef           PrimRef

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

        -- | A mutable reference.
        | TyConPrimRef
        deriving (Eq, Ord, Show)


-- OpPrimRef ------------------------------------------------------------------
-- | Mutable References.
data PrimRef
        = PrimRefAllocRef     -- ^ Allocate a reference.
        | PrimRefReadRef      -- ^ Read a reference.
        | PrimRefWriteRef     -- ^ Write to a reference.
        deriving (Eq, Ord, Show)

