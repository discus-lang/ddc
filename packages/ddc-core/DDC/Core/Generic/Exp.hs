
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
import DDC.Type.Exp


---------------------------------------------------------------------------------------------------
-- | Generic expression representation.
data GExp b u c p
        -- | Value variable or primitive operator.
        = XVar     !u

        -- | Data constructor.
        | XCon     !(DaCon c)

        -- | Primitive operator or literal.
        | XPrim    !p

        -- | Type abstraction (level-1 abstration).
        | XLAM     !b               !(GExp b u c p)

        -- | Value and witness abstraction (level-0 abstraction).
        | XLam     !b               !(GExp b u c p)

        -- | Application.
        | XApp     !(GExp  b u c p) !(GExp b u c p)

        -- | Possibly recursive bindings.
        | XLet     !(GLets b u c p) !(GExp b u c p)

        -- | Case branching.
        | XCase    !(GExp  b u c p) ![GAlt b u c p]

        -- | Type casting.
        | XCast    !(GCast b u c p) !(GExp b u c p)

        -- | Type can appear as the argument of an application.
        | XType    !(Type c)

        -- | Witness can appear as the argument of an application.
        | XWitness !(GWitness b u c p)
        deriving Show


-- | Possibly recursive bindings.
data GLets b u c p
        -- | Non-recursive binding.
        = LLet     !b           !(GExp b u c p)

        -- | Recursive binding.
        | LRec     ![(b, GExp b u c p)]

        -- | Introduce a private region variable and witnesses to its properties.
        | LPrivate ![b] !(Maybe (Type c)) ![b]
        deriving Show


-- | Case alternatives.
data GAlt b u c p
        = AAlt !(GPat b u c p) !(GExp b u c p)
        deriving Show


-- | Patterns.
data GPat b u c p
        -- | The default pattern always succeeds.
        = PDefault

        -- | Match a data constructor and bind its arguments.
        | PData !(DaCon c) ![b]
        deriving Show


-- | Type casts.
data GCast b u c p
        -- | Weaken the effect of an expression.
        = CastWeakenEffect   !(Type c)

        -- | Purify the effect of an expression.
        | CastPurify         !(GWitness b u c p)

        -- | Box up a computation, suspending its evaluation and capturing 
        --   its effects in the S computaiton type.
        | CastBox

        -- | Run a computation, releasing its effects into the context.
        | CastRun
        deriving Show


-- | Witnesses.
data GWitness b u c p
        -- | Witness variable.
        = WVar  !u

        -- | Witness constructor.
        | WCon  !(GWiCon b u c p)

        -- | Witness application.
        | WApp  !(GWitness b u c p) !(GWitness b u c p)

        -- | Type can appear as an argument of a witness application.
        | WType !(Type c)
        deriving Show


-- | Witness constructors.
data GWiCon b u c p
        -- | Witness constructors defined in the environment.
        --   In the interpreter we use this to hold runtime capabilities.
        --   The attached type must be closed.
        = WiConBound   !u !(Type c)
        deriving Show

