
module DDC.Core.Blue.Prim.Base
        ( Name          (..)
        , PrimTyCon     (..)
        , PrimArith     (..)
        , PrimCast      (..))
where
import Data.Typeable
import DDC.Core.Salt.Name 
        ( PrimTyCon     (..)
        , PrimArith     (..)
        , PrimCast      (..))


-- | Names of things used in Disciple Core Blue.
data Name
        -- | User defined variables.
        = NameVar               String

        -- | A user defined constructor.
        | NameCon               String

        -- Machine primitives ------------------
        -- | A primitive type constructor.
        | NamePrimTyCon         PrimTyCon

        -- | Primitive arithmetic, logic, comparison and bit-wise operators.
        | NamePrimArith         PrimArith

        -- | Primitive casting between numeric types.
        | NamePrimCast          PrimCast


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

