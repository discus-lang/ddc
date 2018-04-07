{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

-- Generic type expression representation.
module DDC.Source.Discus.Exp.Type.Base
        ( module DDC.Source.Discus.Exp.Bind

          -- * Type Families
        , GTAnnot

          -- * Abstract Syntax
        , GType         (..)
        , GTyCon        (..)
        , TyConPrim     (..)
        , PrimTyCon     (..)
        , TyConDiscus   (..)

          -- * Syntactic Sugar
        , pattern TApp2
        , pattern TApp3
        , pattern TApp4
        , pattern TApp5

        , pattern TVoid
        , pattern TUnit
        , pattern TBot
        , pattern TUnion
        , pattern TPrim
        , pattern TFunExplicit
        , pattern TFunImplicit

        , pattern KData
        , pattern KRegion
        , pattern KEffect

        , pattern TImpl
        , pattern TSusp
        , pattern TRead
        , pattern TWrite
        , pattern TAlloc

        , pattern TBool
        , pattern TNat
        , pattern TInt
        , pattern TSize
        , pattern TWord
        , pattern TFloat
        , pattern TTextLit

        , pattern TVector
        , pattern TFunValue

          -- * Classes
        , ShowGType)
where
import DDC.Source.Discus.Exp.Bind
import DDC.Type.Exp.TyCon

import DDC.Core.Discus  (PrimTyCon     (..))


---------------------------------------------------------------------------------------------------
-- Type functions associated with the language AST.

-- | Yield the type of annotations.
type family GTAnnot    l


---------------------------------------------------------------------------------------------------
-- | Generic type expression representation.
data GType l
        -- | An annotated type.
        = TAnnot     !(GTAnnot l) (GType l)

        -- | Type constructor or literal.
        | TCon       !(GTyCon l)

        -- | Type variable.
        | TVar       !Bound

        -- | Type abstracton.
        | TAbs       !Bind (GType l) (GType l)

        -- | Type application.
        | TApp       !(GType l) (GType l)


-- | Wrapper for primitive constructors that adds the ones
--   common to SystemFω based languages.
data GTyCon l
        -- | The void constructor.
        = TyConVoid

        -- | The unit constructor.
        | TyConUnit

        -- | The explicit function type constructor.
        | TyConFunExplicit

        -- | The implicit function type constructor.
        | TyConFunImplicit

        -- | Take the least upper bound at the given kind.
        | TyConUnion  !(GType l)

        -- | The least element of the given kind.
        | TyConBot    !(GType l)

        -- | The universal quantifier with a parameter of the given kind.
        | TyConForall !(GType l)

        -- | The existential quantifier with a parameter of the given kind.
        | TyConExists !(GType l)

        -- | Primitive constructor.
        | TyConPrim   !TyConPrim

        -- | Bound constructor.
        | TyConBound  !TyConBound


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

        -- | @F#@.       Reified function values.
        | TyConDiscusF

        -- | @U#@.       Explicitly unboxed values.
        | TyConDiscusU
        deriving (Eq, Ord, Show)


---------------------------------------------------------------------------------------------------
-- Patterns for Generic Type Constructors

-- | Applcation of a type to two arguments.
pattern TApp2 t0 t1 t2          = TApp (TApp t0 t1) t2

-- | Applcation of a type to three arguments.
pattern TApp3 t0 t1 t2 t3       = TApp (TApp (TApp t0 t1) t2) t3

-- | Applcation of a type to four arguments.
pattern TApp4 t0 t1 t2 t3 t4    = TApp (TApp (TApp (TApp t0 t1) t2) t3) t4

-- | Applcation of a type to five arguments.
pattern TApp5 t0 t1 t2 t3 t4 t5 = TApp (TApp (TApp (TApp (TApp t0 t1) t2) t3) t4) t5

-- | Representation of the void type.
pattern TVoid                   = TCon TyConVoid

-- | Representation of the unit type.
pattern TUnit                   = TCon TyConUnit

-- | Representation of the bottom type at a given kind.
pattern TBot k                  = TCon (TyConBot k)

-- | Representation of a union of two types.
pattern TUnion k t1 t2          = TApp (TApp (TCon (TyConUnion k)) t1) t2

-- | Representation of primitive type constructors.
pattern TPrim   p               = TCon (TyConPrim p)

-- | Representation of the explicit function type.
pattern TFunExplicit t1 t2      = TApp (TApp (TCon TyConFunExplicit) t1) t2

-- | Representation of the implicit function type.
pattern TFunImplicit t1 t2      = TApp (TApp (TCon TyConFunImplicit) t1) t2


---------------------------------------------------------------------------------------------------
-- Patterns for Kind Constructors

-- | Representation of the Data kind.
pattern KData           = TCon (TyConPrim (TyConPrimKiCon KiConData))

-- | Representation of the Region kind.
pattern KRegion         = TCon (TyConPrim (TyConPrimKiCon KiConRegion))

-- | Representation of the Effect kind.
pattern KEffect         = TCon (TyConPrim (TyConPrimKiCon KiConEffect))


---------------------------------------------------------------------------------------------------
-- Patterns for Type Constructors

-- | Representation of an implication type.
pattern TImpl  t1 t2    = TApp (TApp (TCon (TyConPrim (TyConPrimTwCon TwConImpl))) t1) t2

-- | Representation of a suspension type.
pattern TSusp  tE tA    = TApp (TApp (TCon (TyConPrim (TyConPrimTcCon TcConSusp))) tE) tA

-- | Representation of a read effect.
pattern TRead  tR       = TApp (TCon (TyConPrim (TyConPrimTcCon TcConRead)))  tR

-- | Representation of a write effect.
pattern TWrite tR       = TApp (TCon (TyConPrim (TyConPrimTcCon TcConWrite))) tR

-- | Representation of a alloc effect.
pattern TAlloc tR       = TApp (TCon (TyConPrim (TyConPrimTcCon TcConAlloc))) tR


---------------------------------------------------------------------------------------------------
-- Patterns for Primitive Type Constructors

-- | Primitive `Bool` type.
pattern TBool           = TCon (TyConPrim (TyConPrimTyCon PrimTyConBool))

-- | Primitive `Nat` type.
pattern TNat            = TCon (TyConPrim (TyConPrimTyCon PrimTyConNat))

-- | Primitive `Int` type.
pattern TInt            = TCon (TyConPrim (TyConPrimTyCon PrimTyConInt))

-- | Primitive `Size` type.
pattern TSize           = TCon (TyConPrim (TyConPrimTyCon PrimTyConSize) )

-- | Primitive `WordN` type of the given width.
pattern TWord bits      = TCon (TyConPrim (TyConPrimTyCon (PrimTyConWord bits)))

-- | Primitive `FloatN` type of the given width.
pattern TFloat bits     = TCon (TyConPrim (TyConPrimTyCon (PrimTyConFloat bits)))

-- | Primitive `TextLit` type.
pattern TTextLit        = TCon (TyConPrim (TyConPrimTyCon PrimTyConTextLit))


---------------------------------------------------------------------------------------------------
-- Patterns for Discus Type Constructors

pattern TVector   tR tA = TApp2 (TCon (TyConPrim (TyConPrimDiscus TyConDiscusVector))) tR tA
pattern TFunValue tA    = TApp  (TCon (TyConPrim (TyConPrimDiscus TyConDiscusF)))      tA

---------------------------------------------------------------------------------------------------
deriving instance (Eq (GType l)) => Eq (GTyCon l)

deriving instance
        ( Eq (GTAnnot l),   Eq (GTyCon l))
        => Eq (GType l)

-- | Synonym for show constraints of all language types.
type ShowGType l
        = ( Show l
          , Show (GTAnnot   l))

deriving instance ShowGType l => Show (GType  l)
deriving instance ShowGType l => Show (GTyCon l)

