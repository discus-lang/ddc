
module DDC.Source.Tetra.Prim.Base
        ( Name          (..)
        , TyConTetra    (..)
        , OpStore       (..)
        , PrimTyCon     (..)
        , PrimArith     (..))
where
import DDC.Core.Tetra    
        ( OpStore       (..)
        , PrimTyCon     (..)
        , PrimArith     (..))


-- | Names of things used in Disciple Source Tetra.
data Name
        -- | A user defined variable.
        = NameVar               String

        -- | A user defined constructor.
        | NameCon               String

        -- Baked in things ----------------------
        -- | Baked in data type constructors.
        | NameTyConTetra        TyConTetra

        -- | Baked in store operators.
        | NameOpStore           OpStore

        -- Machine primitives -------------------
        -- | Primitive type cosntructors.
        | NamePrimTyCon         PrimTyCon

        -- | Primitive arithmetic, logic and comparison.
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

        -- Inference ----------------------------
        -- | A hole used during type inference.
        | NameHole              
        deriving (Eq, Ord, Show)


-- TyConTetra ----------------------------------------------------------------
-- | Baked-in type constructors.
data TyConTetra
        -- | @Ref#@.    Mutable reference.
        = TyConTetraRef

        -- | @TupleN#@. Tuples.
        | TyConTetraTuple Int
        deriving (Eq, Ord, Show)



