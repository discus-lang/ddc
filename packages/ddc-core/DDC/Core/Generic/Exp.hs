{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module DDC.Core.Generic.Exp where


---------------------------------------------------------------------------------------------------
-- | Language tag.
class Language l where
 type Bind    l
 type Bound   l
 type Prim    l
 type Type    l
 type DaCon   l
 type Exp     l
 type Lets    l
 type Alt     l
 type Pat     l
 type Cast    l
 type Witness l
 type WiCon   l


---------------------------------------------------------------------------------------------------
-- | Expression representation.
data RExp l
        -- | Value variable or primitive operator.
        = XVar     !(Bound l)

        -- | Data constructor.
        | XCon     !(DaCon l)

        -- | Primitive operator or literal.
        | XPrim    !(Prim  l)

        -- | Type abstraction (level-1 abstration).
        | XLAM     !(Bind  l)  !(Exp l)

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


-- | Possibly recursive bindings.
data RLets l
        -- | Non-recursive binding.
        = LLet     !(Bind l)    !(Exp l)

        -- | Recursive binding.
        | LRec     ![(Bind l, Exp l)]

        -- | Introduce a private region variable and witnesses to its properties.
        | LPrivate ![Bind l] !(Maybe (Type l)) ![Bind l]


-- | Case alternatives.
data RAlt l
        = AAlt !(Pat l) !(Exp l)


-- | Patterns.
data RPat l
        -- | The default pattern always succeeds.
        = PDefault

        -- | Match a data constructor and bind its arguments.
        | PData !(DaCon l) ![Bind l]


-- | Type casts.
data RCast l
        -- | Weaken the effect of an expression.
        = CastWeakenEffect   !(Type l)

        -- | Purify the effect of an expression.
        | CastPurify         !(Witness l)

        -- | Box up a computation, suspending its evaluation and capturing 
        --   its effects in the S computaiton type.
        | CastBox

        -- | Run a computation, releasing its effects into the context.
        | CastRun


-- | Witnesses.
data RWitness l
        -- | Witness variable.
        = WVar  !(Bound l)

        -- | Witness constructor.
        | WCon  !(WiCon l)

        -- | Witness application.
        | WApp  !(Witness l) !(Witness l)

        -- | Type can appear as an argument of a witness application.
        | WType !(Type l)


-- | Witness constructors.
data RWiCon l
        -- | Witness constructors defined in the environment.
        --   In the interpreter we use this to hold runtime capabilities.
        --   The attached type must be closed.
        = WiConBound   !(Bound l) !(Type l)


---------------------------------------------------------------------------------------------------
-- Show instances.
deriving instance
  ( Show (Bind l),  Show (Bound l), Show (Prim  l), Show (DaCon l)
  , Show (Exp l),   Show (Alt l),   Show (Lets l)
  , Show (Cast  l), Show (Type  l), Show (Witness l))
 => Show (RExp l)

deriving instance 
   (Show (Bind l),  Show (Type l), Show (Exp l))
 => Show (RLets l)

deriving instance 
   (Show (Pat l),   Show (Exp l))
 => Show (RAlt l)

deriving instance 
   (Show (DaCon l), Show (Bind l))
 => Show (RPat l)

deriving instance
   (Show (Type l),  Show (Witness l))
 => Show (RCast l)

deriving instance 
   (Show (Bound l), Show (Type l)) 
 => Show (RWiCon l)

deriving instance
   (Show (Bound l), Show (Type l), Show (Witness l), Show (WiCon l))
 => Show (RWitness l)

