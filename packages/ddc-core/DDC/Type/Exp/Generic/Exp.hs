{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

-- Generic type expression representation.
module DDC.Type.Exp.Generic.Exp 
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
        , pattern TFun
        , pattern TBot
        , pattern TSum
        , pattern TForall
        , pattern TExists
        , pattern TPrim

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
        | TAbs       !(GTBindVar  l) (GType l)

        -- | Type application.
        | TApp       !(GType      l) (GType l)


pattern TApp2 t0 t1 t2          = TApp (TApp t0 t1) t2
pattern TApp3 t0 t1 t2 t3       = TApp (TApp (TApp t0 t1) t2) t3
pattern TApp4 t0 t1 t2 t3 t4    = TApp (TApp (TApp (TApp t0 t1) t2) t3) t4
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

        -- | The function constructor.
        | TyConFun

        -- | Take the least upper bound at the given kind.
        --   TODO: Rename this to Union, as it's really a type constructor for a union.
        | TyConSum    !(GType l)

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

-- | Representation of the function type.
pattern TFun t1 t2      = TApp (TApp (TCon TyConFun) t1) t2

-- | Representation of the bottom type at a given kind.
pattern TBot k          = TCon (TyConBot k)

-- | Representation of a sum of two types.
pattern TSum k t1 t2    = TApp (TApp (TCon (TyConSum k)) t1) t2

-- | Representation of forall quantified types.
pattern TForall k b t   = TApp (TCon (TyConForall k)) (TAbs b t)

-- | Representation of exists quantified types.
pattern TExists k b t   = TApp (TCon (TyConExists k)) (TAbs b t)

-- | Representation of primitive type constructors.
pattern TPrim   p       = TCon (TyConPrim p)


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

