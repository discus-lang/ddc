{-# OPTIONS_HADDOCK hide #-}
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
        | XCon     !(DaCon  l (T.Type l))

        -- | Value or Witness variable (level-0).
        | XVar     !(GBound l)

        -- | Function abstraction.
        | XAbs     !(GParam l) !(GExp l)

        -- | Function application.
        | XApp     !(GExp  l)  !(GArg l)

        -- | Possibly recursive bindings.
        | XLet     !(GLets l)  !(GExp l)

        -- | Case branching.
        | XCase    !(GExp  l)  ![GAlt l]

        -- | Type casting.
        | XCast    !(GCast l)  !(GExp l)


pattern XLAM b x = XAbs (MType b) x
pattern XLam b x = XAbs (MTerm b) x


-- | Parameter of an abstraction.
data GParam l
        = MType    !(GBind l)           -- ^ Type binder.
        | MTerm    !(GBind l)           -- ^ Term binder.


-- | Argumenet of an application.
data GArg l
        = RType    !(T.Type l)          -- ^ Type argument.
        | RExp     !(GExp l)            -- ^ Term argument.
        | RWitness !(GWitness l)        -- ^ Witness argument


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
        | PData !(DaCon l (T.Type l)) ![GBind l]


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

        -- | Primitive cast operators.
        | CastPrim            GCastPrim


-- | Primitive cast operators.
--   Each instance can be assigned a regular function type,
--   but the result type depends polytypically on the parameter types.
data GCastPrim
        -- | Project out a single field from a record.
        --     project# (x1,x2,x3,x4)# (x3)#
        --      : [a b c d: Data]. (x1,x2,x3,x4)# a b c d -> c
        = CastPrimProject

        -- | Shuffle fields in a record.
        --     shuffle# (x1,x2,x3,x4)# (x1,x2,x4)#
        --      : [a b c d: Data]. (x1,x2,x3,x4)# a b c d -> (x1,x2,x4)# a b d
        | CastPrimShuffle

        -- | Combine two records into a compound one.
        --     combine# (x1,x3)# (x2,x4)# (x1,x2,x3,x4)#
        --      : [a b c d: Data]. (x1,x3)# a b -> (x2,x4)# c d -> (x1,x2,x3,x4)# a c b d
        | CastPrimCombine


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
deriving instance ShowLanguage l => Show (GParam   l)
deriving instance ShowLanguage l => Show (GArg     l)
deriving instance ShowLanguage l => Show (GLets    l)
deriving instance ShowLanguage l => Show (GAlt     l)
deriving instance ShowLanguage l => Show (GPat     l)
deriving instance ShowLanguage l => Show (GCast    l)
deriving instance ShowLanguage l => Show (GWitness l)
deriving instance ShowLanguage l => Show (GWiCon   l)
deriving instance Show GCastPrim

