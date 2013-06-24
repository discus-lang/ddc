
module DDC.Core.Exp.Base where
import DDC.Core.Exp.WiCon
import DDC.Core.DaCon
import DDC.Type.Exp
import DDC.Type.Sum             ()


-- Values ---------------------------------------------------------------------
-- | Well-typed expressions produce `Data` values when evaluated, 
--   and their types aways have kind '*' (Data)
data Exp a n
        -- | Value variable   or primitive operation.
        = XVar  !a  !(Bound n)

        -- | Data constructor or literal.
        | XCon  !a  !(DaCon n)

        -- | Type abstraction (level-1).
        | XLAM  !a  !(Bind n)   !(Exp a n)

        -- | Value and Witness abstraction (level-0).
        | XLam  !a  !(Bind n)   !(Exp a n)

        -- | Application.
        | XApp  !a  !(Exp a n)  !(Exp a n)

        -- | Possibly recursive bindings.
        | XLet  !a  !(Lets a n) !(Exp a n)

        -- | Case branching.
        | XCase !a  !(Exp a n)  ![Alt a n]

        -- | Type cast.
        | XCast !a  !(Cast a n) !(Exp a n)

        -- | Type can appear as the argument of an application.
        | XType    !(Type n)

        -- | Witness can appear as the argument of an application.
        | XWitness !(Witness a n)
        deriving (Show, Eq)

deriving instance Eq n => Eq (DaCon n)


-- | Type casts.
data Cast a n
        -- | Weaken the effect of an expression.
        --   The given effect is added to the effect
        --   of the body.
        = CastWeakenEffect  !(Effect n)
        
        -- | Weaken the closure of an expression.
        --   The closures of these expressions are added to the closure
        --   of the body.
        | CastWeakenClosure ![Exp a n]

        -- | Purify the effect (action) of an expression.
        | CastPurify !(Witness a n)

        -- | Forget about the closure (sharing) of an expression.
        | CastForget !(Witness a n)
        deriving (Show, Eq)


-- | Possibly recursive bindings.
data Lets a n
        -- | Non-recursive expression binding.
        = LLet    !(Bind n) !(Exp a n)

        -- | Recursive binding of lambda abstractions.
        | LRec    ![(Bind n, Exp a n)]

        -- | Bind a local region variable,
        --   and witnesses to its properties.
        | LLetRegions ![Bind n] ![Bind n]
        
        -- | Holds a region handle during evaluation.
        | LWithRegion !(Bound n)
        deriving (Show, Eq)


-- | Case alternatives.
data Alt a n
        = AAlt !(Pat n) !(Exp a n)
        deriving (Show, Eq)


-- | Pattern matching.
data Pat n
        -- | The default pattern always succeeds.
        = PDefault
        
        -- | Match a data constructor and bind its arguments.
        | PData !(DaCon n) ![Bind n]
        deriving (Show, Eq)
        

-- Witness --------------------------------------------------------------------
-- | When a witness exists in the program it guarantees that a
--   certain property of the program is true.
data Witness a n
        -- | Witness variable.
        = WVar  a !(Bound n)
        
        -- | Witness constructor.
        | WCon  a !(WiCon n)
        
        -- | Witness application.
        | WApp  a !(Witness a n) !(Witness a n)

        -- | Joining of witnesses.
        | WJoin a !(Witness a n) !(Witness a n)

        -- | Type can appear as the argument of an application.
        | WType a !(Type n)
        deriving (Show, Eq)

