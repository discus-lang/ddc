{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

-- Generic type expression representation.
module DDC.Source.Discus.Exp.Type.Base
        ( -- * Type Families
          GTAnnot
        , GTBindVar, GTBoundVar
        , GTBindCon, GTBoundCon
        , GTPrim

          -- * Abstract Syntax
        , GType         (..)
        , GTyCon        (..)

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

          -- * Classes
        , ShowGType)
where


---------------------------------------------------------------------------------------------------
-- Type functions associated with the language AST.

-- | Yield the type of annotations.
type family GTAnnot    l

-- | Yield the type of binding occurrences of variables.
type family GTBindVar  l

-- | Yield the type of bound occurrences of variables.
type family GTBoundVar l

-- | Yield the type of binding occurrences of constructors.
type family GTBindCon  l

-- | Yield the type of bound occurrences of constructors.
type family GTBoundCon l

-- | Yield the type of primitive type names.
type family GTPrim     l


---------------------------------------------------------------------------------------------------
-- | Generic type expression representation.
data GType l
        -- | An annotated type.
        = TAnnot     !(GTAnnot l) (GType l)

        -- | Type constructor or literal.
        | TCon       !(GTyCon l)

        -- | Type variable.
        | TVar       !(GTBoundVar l)

        -- | Type abstracton.
        | TAbs       !(GTBindVar l) (GType l) (GType l)

        -- | Type application.
        | TApp       !(GType l) (GType l)


-- | Applcation of a type to two arguments.
pattern TApp2 t0 t1 t2          = TApp (TApp t0 t1) t2

-- | Applcation of a type to three arguments.
pattern TApp3 t0 t1 t2 t3       = TApp (TApp (TApp t0 t1) t2) t3

-- | Applcation of a type to four arguments.
pattern TApp4 t0 t1 t2 t3 t4    = TApp (TApp (TApp (TApp t0 t1) t2) t3) t4

-- | Applcation of a type to five arguments.
pattern TApp5 t0 t1 t2 t3 t4 t5 = TApp (TApp (TApp (TApp (TApp t0 t1) t2) t3) t4) t5


deriving instance
        ( Eq (GTAnnot l),   Eq (GTyCon l)
        , Eq (GTBindVar l), Eq (GTBoundVar l))
        => Eq (GType l)


---------------------------------------------------------------------------------------------------
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
        | TyConPrim   !(GTPrim l)

        -- | Bound constructor.
        | TyConBound  !(GTBoundCon l)


deriving instance
        (Eq (GType l), Eq (GTPrim l), Eq (GTBoundCon l))
        => Eq (GTyCon l)


---------------------------------------------------------------------------------------------------
-- | Representation of the void type.
pattern TVoid           = TCon TyConVoid

-- | Representation of the unit type.
pattern TUnit           = TCon TyConUnit

-- | Representation of the bottom type at a given kind.
pattern TBot k          = TCon (TyConBot k)

-- | Representation of a union of two types.
pattern TUnion k t1 t2  = TApp (TApp (TCon (TyConUnion k)) t1) t2

-- | Representation of primitive type constructors.
pattern TPrim   p       = TCon (TyConPrim p)

-- | Representation of the explicit function type.
pattern TFunExplicit t1 t2  = TApp (TApp (TCon TyConFunExplicit) t1) t2

-- | Representation of the implicit function type.
pattern TFunImplicit t1 t2  = TApp (TApp (TCon TyConFunImplicit) t1) t2


---------------------------------------------------------------------------------------------------
-- | Synonym for show constraints of all language types.
type ShowGType l
        = ( Show l
          , Show (GTAnnot   l)
          , Show (GTBindVar l), Show (GTBoundVar l)
          , Show (GTBindCon l), Show (GTBoundCon l)
          , Show (GTPrim    l))

deriving instance ShowGType l => Show (GType  l)
deriving instance ShowGType l => Show (GTyCon l)

