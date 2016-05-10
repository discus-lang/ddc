{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

-- Generic type expression representation.
module DDC.Type.Exp.Generic.Exp where


-------------------------------------------------------------------------------
-- Type functions associated with the language definition.

-- | Yield the type of annotations.
type family GAnnot    l

-- | Yield the type of binding occurrences of variables.
type family GBindVar  l

-- | Yield the type of bound occurrences of variables.
type family GBoundVar l

-- | Yield the type of binding occurrences of constructors.
type family GBindCon  l

-- | Yield the type of bound occurrences of constructors.
type family GBoundCon l

-- | Yield the type of primitive names for language @l@.
type family GPrim     l


-------------------------------------------------------------------------------
-- | Generic type expression representation.
data GType l
        -- | An annotated type.
        = TAnnot     !(GAnnot l) (GType l)

        -- | Type constructor or literal.
        | TCon       !(GTyCon l)

        -- | Type variable.
        | TVar       !(GBoundVar l)

        -- | Type abstracton.
        | TAbs       !(GBindVar  l) (GType l)

        -- | Type application.
        | TApp       !(GType     l) (GType l)


-------------------------------------------------------------------------------
-- | Wrapper for primitive constructors that adds the ones
--   common to SystemFω based languages.
data GTyCon l
        -- | The function constructor.
        = TyConFun

        -- | The unit constructor.
        | TyConUnit

        -- | The void constructor.
        | TyConVoid

        -- | Take the least upper bound at the given kind.
        | TyConSum    !(GType l)

        -- | The least element of the given kind.
        | TyConBot    !(GType l)

        -- | The universal quantifier with a parameter of the given kind.
        | TyConForall !(GType l)

        -- | The existential quantifier with a parameter of the given kind.
        | TyConExists !(GType l)

        -- | Primitive constructor.
        | TyConPrim   !(GPrim l)

        -- | Bound constructor.
        | TyConBound  !(GBoundCon l)


-------------------------------------------------------------------------------
-- | Representation of the function type.
pattern TFun            = TCon TyConFun

-- | Representation of the unit type.
pattern TUnit           = TCon TyConUnit

-- | Representation of the void type.
pattern TVoid           = TCon TyConVoid

-- | Representatino of the bottom type at a given kind.
pattern TBot k          = TCon (TyConBot k)

-- | Representation of forall quantified types.
pattern TForall k b t   = TApp (TCon (TyConForall k)) (TAbs b t)

-- | Representation of exists quantified types.
pattern TExists k b t   = TApp (TCon (TyConExists k)) (TAbs b t)

-- | Representation of primitive type constructors.
pattern TPrim   p       = TCon (TyConPrim p)


-------------------------------------------------------------------------------
-- | Synonym for show constraints of all language types.
type ShowLanguage l
        = ( Show l
          , Show (GAnnot   l)
          , Show (GBindVar l), Show (GBoundVar l)
          , Show (GBindCon l), Show (GBoundCon l)
          , Show (GPrim    l))

deriving instance ShowLanguage l => Show (GType  l)
deriving instance ShowLanguage l => Show (GTyCon l)

