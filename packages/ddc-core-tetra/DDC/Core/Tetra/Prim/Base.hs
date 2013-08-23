
module DDC.Core.Tetra.Prim.Base
        ( Name          (..)
        , TyConData     (..)
        , OpStore       (..)
        , PrimTyCon     (..)
        , PrimArith     (..))
where
import Data.Typeable
import DDC.Core.Salt.Name.PrimTyCon
import DDC.Core.Salt.Name.PrimArith


-- | Names of things used in Disciple Core Tetra.
data Name
        -- | User defined variables.
        = NameVar               String

        -- | A user defined constructor.
        | NameCon               String

        -- Baked-in data types -----------------
        | NameTyConData         TyConData

        -- Baked-in operators ------------------
        | NameOpStore           OpStore

        -- Machine primitives ------------------
        -- | A primitive type constructor.
        | NamePrimTyCon         PrimTyCon

        -- | Primitive arithmetic, logic, comparison and bit-wise operators.
        | NamePrimArith         PrimArith

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


-- TyConData------------------------------------------------------------------
-- | Baked-in data types.
data TyConData
        -- | A mutable reference.
        = TyConDataRef
        deriving (Eq, Ord, Show)


-- OpStore -------------------------------------------------------------------
-- | Mutable References.
data OpStore
        = OpStoreAllocRef     -- ^ Allocate a reference.
        | OpStoreReadRef      -- ^ Read a reference.
        | OpStoreWriteRef     -- ^ Write to a reference.
        deriving (Eq, Ord, Show)

