
module DDC.Source.Discus.Exp.Type.Prim
        ( TyConPrim     (..)
        , TyConDiscus   (..))
where
import DDC.Core.Discus  (PrimTyCon     (..))
import DDC.Type.Exp.TyCon


-- | Primitive machine types.
data TyConPrim
        -- | Primitive sort constructors.
        = TyConPrimSoCon        !SoCon

        -- | Primitive kind constructors.
        | TyConPrimKiCon        !KiCon

        -- | Primitive witness type constructors.
        | TyConPrimTwCon        !TwCon

        -- | Other type constructors at the spec level.
        | TyConPrimTcCon        !TcCon

        -- | Primitive machine type constructors.
        | TyConPrimTyCon        !PrimTyCon

        -- | Primtive type constructors specific to the Discus fragment.
        | TyConPrimDiscus       !TyConDiscus
        deriving (Eq, Ord, Show)


-- | Primitive type constructors specific to the Discus language fragment.
data TyConDiscus
        -- | @TupleN#@. Tuples.
        = TyConDiscusTuple !Int

        -- | @Vector#@. Vectors.
        | TyConDiscusVector

        -- | @F#@. Reified function values.
        | TyConDiscusF

        -- | @U#@. Explicitly unboxed values.
        | TyConDiscusU
        deriving (Eq, Ord, Show)


