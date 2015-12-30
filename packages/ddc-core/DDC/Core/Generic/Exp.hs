{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

-- | Generic expression representation.
--
--   Each of these types is polymorphic in the names used for
--   (b)inding occurrences, bo(u)nd occurrences, (c)onstructors and (p)rimitives.
--
--   Clients of this module are expected to defined their own type synonyms
--   for 'Exp', 'Lets' and so on that instantiate each of these data types to
--   use more specific types for names.
--
module DDC.Core.Generic.Exp where
import DDC.Core.Exp.DaCon
import qualified DDC.Type.Exp   as T

---------------------------------------------------------------------------------------------------
class Language l where
 type Annot l
 type Bind  l    
 type Bound l
 type Prim  l


---------------------------------------------------------------------------------------------------
-- | Generic expression representation.
data GExp l
        = XAnnot   !(Annot l)  !(GExp l)

        -- | Value variable or primitive operator.
        | XVar     !(Bound l)

        -- | Data constructor.
        | XCon     !(DaCon l)

        -- | Primitive operator or literal.
        | XPrim    !(Prim  l)

        -- | Type abstraction (level-1 abstration).
        | XLAM     !(Bind  l)  !(GExp l)

        -- | Value and witness abstraction (level-0 abstraction).
        | XLam     !(Bind  l)  !(GExp l)

        -- | Application.
        | XApp     !(GExp  l)  !(GExp l)

        -- | Possibly recursive bindings.
        | XLet     !(GLets l)  !(GExp l)

        -- | Case branching.
        | XCase    !(GExp  l)  ![GAlt l]

        -- | Type casting.
        | XCast    !(GCast l)  !(GExp l)

        -- | Type can appear as the argument of an application.
        | XType    !(T.Type  l)

        -- | Witness can appear as the argument of an application.
        | XWitness !(GWitness l)


-- | Possibly recursive bindings.
data GLets l
        -- | Non-recursive binding.
        = LLet     !(Bind l)  !(GExp l)

        -- | Recursive binding.
        | LRec     ![(Bind l, GExp l)]

        -- | Introduce a private region variable and witnesses to its properties.
        | LPrivate ![Bind l] !(Maybe (T.Type l)) ![Bind l]


-- | Case alternatives.
data GAlt l
        = AAlt !(GPat l) !(GExp l)


-- | Patterns.
data GPat l
        -- | The default pattern always succeeds.
        = PDefault

        -- | Match a data constructor and bind its arguments.
        | PData !(DaCon l) ![Bind l]


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
        = WVar  !(Bound l)

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
        = WiConBound   !(Bound l) !(T.Type l)


---------------------------------------------------------------------------------------------------
-- Show instances.

deriving instance 
        (Show l, Show (Annot l), Show (Bind l), Show (Bound l), Show (Prim l)) 
        => Show (GExp l)

deriving instance 
        (Show l, Show (Annot l), Show (Bind l), Show (Bound l), Show (Prim l)) 
        => Show (GLets l)

deriving instance 
        (Show l, Show (Annot l), Show (Bind l), Show (Bound l), Show (Prim l)) 
        => Show (GAlt l)

deriving instance 
        (Show l, Show (Annot l), Show (Bind l), Show (Bound l), Show (Prim l)) 
        => Show (GPat l)

deriving instance 
        (Show l, Show (Annot l), Show (Bind l), Show (Bound l), Show (Prim l)) 
        => Show (GCast l)

deriving instance 
        (Show l, Show (Annot l), Show (Bind l), Show (Bound l), Show (Prim l)) 
        => Show (GWitness l)

deriving instance 
        (Show l, Show (Annot l), Show (Bind l), Show (Bound l), Show (Prim l)) 
        => Show (GWiCon l)

