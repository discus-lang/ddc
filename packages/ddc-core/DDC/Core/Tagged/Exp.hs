{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module DDC.Core.Tagged.Exp where


---------------------------------------------------------------------------------------------------
-- | Language tag.
class Language l where
 type Bind    l
 type Bound   l
 type Type    l
 type DaCon   l
 type Exp     l
 type Lets    l
 type Alt     l
 type Pat     l
 type Cast    l
 type Witness l
 type WiCon   l


-- | Language AST using direct recursion to the nodes.
data Direct b u 

instance Language (Direct b u) where
 type Bind    (Direct b u)      = b
 type Bound   (Direct b u)      = u
 type Type    (Direct b u)      = String
 type DaCon   (Direct b u)      = String
 type Exp     (Direct b u)      = DExp     (Direct b u)
 type Lets    (Direct b u)      = DLets    (Direct b u)
 type Alt     (Direct b u)      = DAlt     (Direct b u)
 type Pat     (Direct b u)      = DPat     (Direct b u)
 type Cast    (Direct b u)      = DCast    (Direct b u)
 type Witness (Direct b u)      = DWitness (Direct b u)
 type WiCon   (Direct b u)      = DWiCon   (Direct b u)


-- | Language AST using annotated expression nodes.
data Annot a b u

data DAnnot a x = XAnnot a x

instance Language (Annot a b u) where
 type Bind    (Annot a b u)     = b
 type Bound   (Annot a b u)     = u
 type Type    (Annot a b u)     = String
 type DaCon   (Annot a b u)     = String
 type Exp     (Annot a b u)     = DAnnot a (DExp (Annot a b u))
 type Lets    (Annot a b u)     = DLets    (Annot a b u)
 type Alt     (Annot a b u)     = DAlt     (Annot a b u)
 type Pat     (Annot a b u)     = DPat     (Annot a b u)
 type Cast    (Annot a b u)     = DCast    (Annot a b u)
 type Witness (Annot a b u)     = DWitness (Annot a b u)
 type WiCon   (Annot a b u)     = DWiCon   (Annot a b u)


---------------------------------------------------------------------------------------------------
-- | Expressions,
--   indexed by language tag.
data DExp l
        -- | Value variable or primitive operator.
        = XVar     !(Bound l)

        -- | Data constructor or literal.
        | XCon     !(DaCon l)

        -- | Type abstraction (level-1 abstration).
        | XLAM     !(Bind  l)

        -- | Value and witness abstraction (level-0 abstraction).
        | XLam     !(Bind  l)  !(Exp l)

        -- | Application.
        | XApp     !(Exp   l)  !(Exp l)

        -- | Possibly recursive bindings.
        | XLet     !(Lets  l)  !(Exp l)

        -- | Case branching.
        | XCase    !(Exp   l)  ![Alt l]

        -- | Type casting.
        | XCast    !(Cast  l)  !(Exp l)

        -- | Type can appear as the argument of an application.
        | XType    !(Type  l)

        -- | Witness can appear as the argument of an application.
        | XWitness !(Witness l)


-- | Possibly recursive bindings,
--   indexed by language tag.
data DLets l
        -- | Non-recursive binding.
        = LLet     !(Bind l)    !(Exp l)

        -- | Recursive binding.
        | LRec     ![(Bind l, Exp l)]

        -- | Introduce a private region variable and witnesses to its properties.
        | LPrivate ![Bind l] !(Maybe (Type l)) ![Bind l]


-- | Case alternatives,
--   indexed by language tag.
data DAlt l
        = AAlt !(Pat l) !(Exp l)


-- | Patterns, 
--   indexed by language tag.
data DPat l
        -- | The default pattern always succeeds.
        = PDefault

        -- | Match a data constructor and bind its arguments.
        | PData !(DaCon l) ![Bind l]



-- | Type casts,
--   indexed by language tag.
data DCast l
        -- | Weaken the effect of an expression.
        = CastWeakenEffect   !(Type l)

        -- | Purify the effect of an expression.
        | CastPurify

        -- | Box up a computation, suspending its evaluation and capturing 
        --   its effects in the S computaiton type.
        | CastBox

        -- | Run a computation, releasing its effects into the context.
        | CastRun


-- | Witnesses,
--   indexed by language tag.
data DWitness l
        -- | Witness variable.
        = WVar  !(Bound l)

        -- | Witness constructor.
        | WCon  !(WiCon l)

        -- | Witness application.
        | WApp  !(Witness l) !(Witness l)

        -- | Type can appear as an argument of a witness application.
        | WType !(Type l)


-- | Witness constructors.
data DWiCon l
        -- | Witness constructors defined in the environment.
        --   In the interpreter we use this to hold runtime capabilities.
        --   The attached type must be closed.
        = WiConBound   !(Bound l) !(Type l)


---------------------------------------------------------------------------------------------------
-- Show instances.
deriving instance
  ( Show (Bind l),  Show (Bound l), Show (DaCon l), Show (Exp l), Show (Alt l)
  , Show (Lets l),  Show (Cast  l), Show (Type  l), Show (Witness l))
 => Show (DExp l)

deriving instance 
   (Show (Bind l),  Show (Type l), Show (Exp l))
 => Show (DLets l)

deriving instance 
   (Show (Pat l),   Show (Exp l))
 => Show (DAlt l)

deriving instance 
   (Show (DaCon l), Show (Bind l))
 => Show (DPat l)

deriving instance
   (Show (Type l))
 => Show (DCast l)

deriving instance 
   (Show (Bound l), Show (Type l)) 
 => Show (DWiCon l)

deriving instance
   (Show (Bound l), Show (Type l), Show (Witness l), Show (WiCon l))
 => Show (DWitness l)

