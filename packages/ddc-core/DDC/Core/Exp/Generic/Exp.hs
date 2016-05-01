{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

-- | Generic term expression representation.
module DDC.Core.Exp.Generic.Exp where
import DDC.Core.Exp.DaCon
import qualified DDC.Type.Exp   as T


-------------------------------------------------------------------------------
-- | Type functions associated with a language definition.
--
--   These produce the types used for annotations, bindings, bound occurrences
--   and primitives for that language.
--
type family GAnnot l
type family GBind  l    
type family GBound l
type family GPrim  l


-------------------------------------------------------------------------------
-- | Generic term expression representation.
data GExp l
        -- | An annotated expression.
        = XAnnot   !(GAnnot l)  !(GExp l)

        -- | Primitive operator or literal.
        | XPrim    !(GPrim  l)

        -- | Data constructor.
        | XCon     !(DaCon l)

        -- | Value or Witness variable (level-0).
        | XVar     !(GBound l)

        -- | Function abstraction.
        | XAbs     !(GAbs  l)  !(GExp l)

        -- | Function application.
        | XApp     !(GExp  l)  !(GArg l)

        -- | Possibly recursive bindings.
        | XLet     !(GLets l)  !(GExp l)

        -- | Case branching.
        | XCase    !(GExp  l)  ![GAlt l]

        -- | Type casting.
        | XCast    !(GCast l)  !(GExp l)


-- | Abstractions.
--
--   This indicates what sort of object is being abstracted over in an XAbs.
--
data GAbs l
        -- | Level-1 abstraction (spec)
        = ALAM     !(GBind l)

        -- | Level-0 abstraction (value and witness)
        | ALam     !(GBind l)

pattern XLAM b x = XAbs (ALAM b) x
pattern XLam b x = XAbs (ALam b) x


-- | Arguments.
--
--   Carries an argument that can be supplied to a function.
--
data GArg l
        -- | Type argument.
        = RType    !(T.Type l)

        -- | Value argument.
        | RExp     !(GExp l)

        -- | Witness argument.
        | RWitness !(GWitness l)



-- | Possibly recursive bindings.
data GLets l
        -- | Non-recursive binding.
        = LLet     !(GBind l)  !(GExp l)

        -- | Recursive binding.
        | LRec     ![(GBind l, GExp l)]

        -- | Introduce a private region variable and witnesses to
        --   its properties.
        | LPrivate ![GBind l] !(Maybe (T.Type l)) ![GBind l]


-- | Case alternatives.
data GAlt l
        = AAlt !(GPat l) !(GExp l)


-- | Patterns.
data GPat l
        -- | The default pattern always succeeds.
        = PDefault

        -- | Match a data constructor and bind its arguments.
        | PData !(DaCon l) ![GBind l]


-- | Type casts.
data GCast l
        -- | Weaken the effect of an expression.
        = CastWeakenEffect   !(T.Type l)

        -- | Purify the effect of an expression.
        | CastPurify         !(GWitness l)

        -- | Box up a computation, suspending its evaluation and capturing 
        --   its effects in the S computaiton type.
        | CastBox

        -- | Run a computation, releasing its effects into the context.
        | CastRun


-- | Witnesses.
data GWitness l
        -- | Witness variable.
        = WVar  !(GBound l)

        -- | Witness constructor.
        | WCon  !(GWiCon l)

        -- | Witness application.
        | WApp  !(GWitness l) !(GWitness l)

        -- | Type can appear as an argument of a witness application.
        | WType !(T.Type l)


-- | Witness constructors.
data GWiCon l
        -- | Witness constructors defined in the environment.
        --   In the interpreter we use this to hold runtime capabilities.
        --   The attached type must be closed.
        = WiConBound   !(GBound l) !(T.Type l)


-------------------------------------------------------------------------------
-- | Synonym for Show constraints of all language types.
type ShowLanguage l
        = ( Show l
          , Show (GAnnot l)
          , Show (GBind l), Show (GBound l)
          , Show (GPrim l))

deriving instance ShowLanguage l => Show (GExp     l)
deriving instance ShowLanguage l => Show (GAbs     l)
deriving instance ShowLanguage l => Show (GArg     l)
deriving instance ShowLanguage l => Show (GLets    l)
deriving instance ShowLanguage l => Show (GAlt     l)
deriving instance ShowLanguage l => Show (GPat     l)
deriving instance ShowLanguage l => Show (GCast    l)
deriving instance ShowLanguage l => Show (GWitness l)
deriving instance ShowLanguage l => Show (GWiCon   l)

