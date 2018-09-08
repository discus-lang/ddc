{-# OPTIONS_HADDOCK hide #-}

-- Generic type expression representation.
module DDC.Source.Discus.Exp.Type.Base
        ( module DDC.Source.Discus.Exp.Bind
        , module DDC.Data.Label

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
        , pattern TFunValue)
where
import DDC.Source.Discus.Exp.Type.Prim
import DDC.Source.Discus.Exp.Bind
import DDC.Type.Exp.TyCon
import DDC.Core.Discus  (PrimTyCon     (..))
import DDC.Data.Label


---------------------------------------------------------------------------------------------------
-- | Generic type expression representation.
data GType a
        ---------------------------------------------------
        -- Core Language Constructs.

        -- | An annotated type.
        = TAnnot !a (GType a)

        -- | Type constructor or literal.
        | TCon  !(GTyCon a)

        -- | Type variable.
        | TVar  !Bound

        -- | Type abstracton.
        | TAbs  !Bind (GType a) (GType a)

        -- | Type application.
        | TApp  !(GType a) (GType a)

        -- | Generic row type, used for tuples, records and variants.
        | TRow  ![(Label, GType a)]

        ---------------------------------------------------
        -- Sugar Constructs.
        --   These are eliminated when desugaring onto core.

        -- | Tuple type which desugars onto a T# row.
        | TTuple   ![(Label, GType a)]

        -- | Record type which desugars onto R# row.
        | TRecord  ![(Label, GType a)]

        -- | Variant type which desugars onto V# row.
        | TVariant ![(Label, GType a)]


-- | Wrapper for primitive constructors that adds the ones
--   common to SystemFω based languages.
data GTyCon a
        -- | The void constructor.
        = TyConVoid

        -- | The unit constructor.
        | TyConUnit

        -- | The explicit function type constructor.
        | TyConFunExplicit

        -- | The implicit function type constructor.
        | TyConFunImplicit

        -- | Take the least upper bound at the given kind.
        | TyConUnion  !(GType a)

        -- | The least element of the given kind.
        | TyConBot    !(GType a)

        -- | The universal quantifier with a parameter of the given kind.
        | TyConForall !(GType a)

        -- | The existential quantifier with a parameter of the given kind.
        | TyConExists !(GType a)

        -- | Primitive constructor.
        | TyConPrim   !TyConPrim

        -- | Bound constructor.
        | TyConBound  !TyConBound


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
deriving instance Eq a   => Eq   (GTyCon a)
deriving instance Eq a   => Eq   (GType a)

deriving instance Show a => Show (GType a)
deriving instance Show a => Show (GTyCon a)

