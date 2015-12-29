
module DDC.Core.Generic.Exp where
import DDC.Core.Exp.DaCon
import DDC.Type.Exp

---------------------------------------------------------------------------------------------------
-- | Expression representation.
data Exp b u c p
        -- | Value variable or primitive operator.
        = XVar     !u

        -- | Data constructor.
        | XCon     !(DaCon c)

        -- | Primitive operator or literal.
        | XPrim    !p

        -- | Type abstraction (level-1 abstration).
        | XLAM     !b               !(Exp b u c p)

        -- | Value and witness abstraction (level-0 abstraction).
        | XLam     !b               !(Exp b u c p)

        -- | Application.
        | XApp     !(Exp  b u c p)  !(Exp b u c p)

        -- | Possibly recursive bindings.
        | XLet     !(Lets b u c p)  !(Exp b u c p)

        -- | Case branching.
        | XCase    !(Exp  b u c p)  ![Alt b u c p]

        -- | Type casting.
        | XCast    !(Cast b u c p)  !(Exp b u c p)

        -- | Type can appear as the argument of an application.
        | XType    !(Type c)

        -- | Witness can appear as the argument of an application.
        | XWitness !(Witness b u c p)
        deriving Show


-- | Possibly recursive bindings.
data Lets b u c p
        -- | Non-recursive binding.
        = LLet     !b           !(Exp b u c p)

        -- | Recursive binding.
        | LRec     ![(b, Exp b u c p)]

        -- | Introduce a private region variable and witnesses to its properties.
        | LPrivate ![b] !(Maybe (Type c)) ![b]
        deriving Show


-- | Case alternatives.
data Alt b u c p
        = AAlt !(Pat b u c p) !(Exp b u c p)
        deriving Show


-- | Patterns.
data Pat b u c p
        -- | The default pattern always succeeds.
        = PDefault

        -- | Match a data constructor and bind its arguments.
        | PData !(DaCon c) ![b]
        deriving Show


-- | Type casts.
data Cast b u c p
        -- | Weaken the effect of an expression.
        = CastWeakenEffect   !(Type c)

        -- | Purify the effect of an expression.
        | CastPurify         !(Witness b u c p)

        -- | Box up a computation, suspending its evaluation and capturing 
        --   its effects in the S computaiton type.
        | CastBox

        -- | Run a computation, releasing its effects into the context.
        | CastRun
        deriving Show


-- | Witnesses.
data Witness b u c p
        -- | Witness variable.
        = WVar  !u

        -- | Witness constructor.
        | WCon  !(WiCon b u c p)

        -- | Witness application.
        | WApp  !(Witness b u c p) !(Witness b u c p)

        -- | Type can appear as an argument of a witness application.
        | WType !(Type c)
        deriving Show


-- | Witness constructors.
data WiCon b u c p
        -- | Witness constructors defined in the environment.
        --   In the interpreter we use this to hold runtime capabilities.
        --   The attached type must be closed.
        = WiConBound   !u !(Type c)
        deriving Show

