{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

-- Generic type expression representation.
module DDC.Type.Exp.Generic.Exp where


-------------------------------------------------------------------------------
-- | Type functions associated with the language definition.
--
type family GAnnot l
type family GBind  l
type family GBound l
type family GPrim  l


-------------------------------------------------------------------------------
-- | Generic type expression representation.
data GType l
        -- | An annotated expression.
        = TAnnot     !(GAnnot l) (GType l)

        -- | Constructor or literal.
        | TCon       !(GCon   l)

        -- | Type variable.
        | TVar       !(GBound l)

        -- | Type abstracton.
        | TAbs       !(GBind  l) (GType l)

        -- | Type application.
        | TApp       !(GType  l) (GType l)


-------------------------------------------------------------------------------
-- | Wrapper for primitive constructors that adds the ones
--   common to SystemFw based languages.
data GCon l
        -- | The arrow constructor at the given level.
        = TConArr    !Int

        -- | Primitive constructors.
        | TConPrim   !(GPrim l)

        -- | Take the least upper bound at the given kind.
        | TConSum    !(GPrim l)

        -- | The least element of the given kind.
        | TConZero   !(GPrim l)

        -- | The universal quantifier with a parameter of the given kind.
        | TConAll    !(GPrim l)

        -- | The existential quantifier with a parameter of the given kind.
        | TConExists !(GPrim l)


-------------------------------------------------------------------------------
-- | Synonym for show constraints of all language types.
type ShowLanguage l
        = ( Show l
          , Show (GAnnot l)
          , Show (GBind  l), Show (GBound l)
          , Show (GPrim  l))

deriving instance ShowLanguage l => Show (GType l)
deriving instance ShowLanguage l => Show (GCon  l)
