
module DDC.Source.Tetra.Name
        (Name (..))
where
import DDC.Core.Blue    
        ( TyConPrim     (..)
        , OpPrimArith   (..)
        , OpPrimRef     (..))


-- | Names of things used in Disciple Source Tetra.
data Name
        -- | A user defined variable.
        = NameVar               String

        -- | A user defined constructor.
        | NameCon               String


        -- Machine primitives -------------------
        -- | Primitive type cosntructors.
        | NameTyConPrim         TyConPrim        

        -- | Primitive arithmetic, logic and comparison.
        | NameOpPrimArith       OpPrimArith

        -- | Mutable references.
        | NameOpPrimRef         OpPrimRef


        -- Literals -----------------------------
        -- | A boolean literal.
        | NameLitBool           Bool

        -- | A natural literal.
        | NameLitNat            Integer

        -- | An integer literal.
        | NameLitInt            Integer

        -- | A word literal.
        | NameLitWord           Integer Int
        deriving (Eq, Ord, Show)


