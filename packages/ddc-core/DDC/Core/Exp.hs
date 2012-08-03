
-- | Abstract syntax for the Disciple core language.
module DDC.Core.Exp 
        ( module DDC.Type.Exp

          -- * Computation expressions
        , Exp     (..)
        , Cast    (..)
        , Lets    (..)
        , LetMode (..)
        , Alt     (..)
        , Pat     (..)
                        
          -- * Witnesses expressions
        , Witness (..)
        , WiCon   (..)
        , WbCon   (..))
where
import DDC.Type.Exp
import DDC.Type.Sum             ()


-- Values ---------------------------------------------------------------------
-- | Well-typed expressions live in the Data universe, and their types always
--   have kind '*'. 
-- 
--   Expressions do something useful at runtime, and might diverge or cause
--   side effects.
data Exp a n
        -- | Value variable   or primitive operation.
        = XVar  a  (Bound n)

        -- | Data constructor or literal.
        | XCon  a  (Bound n)

        -- | Type abstraction (level-1).
        | XLAM  a  (Bind n)   (Exp a n)

        -- | Value and Witness abstraction (level-0).
        | XLam  a  (Bind n)   (Exp a n)

        -- | Application.
        | XApp  a  (Exp a n)  (Exp a n)

        -- | Possibly recursive bindings.
        | XLet  a  (Lets a n) (Exp a n)

        -- | Case branching.
        | XCase a  (Exp a n)  [Alt a n]

        -- | Type cast.
        | XCast a  (Cast n)   (Exp a n)

        -- | Type can appear as the argument of an application.
        | XType    (Type n)

        -- | Witness can appear as the argument of an application.
        | XWitness (Witness n)
        deriving (Eq, Show)


-- | Type casts.
data Cast n
        -- | Weaken the effect of an expression.
        = CastWeakenEffect  (Effect n)
        
        -- | Weaken the closure of an expression.
        | CastWeakenClosure (Closure n)

        -- | Purify the effect of an expression.
        | CastPurify (Witness n)

        -- | Hide sharing of the closure of an expression.
        | CastForget (Witness n)
        deriving (Eq, Show)


-- | Possibly recursive bindings.
data Lets a n
        -- | Non-recursive expression binding.
        = LLet    (LetMode n) (Bind n) (Exp a n)

        -- | Recursive binding of lambda abstractions.
        | LRec    [(Bind n, Exp a n)]

        -- | Bind a local region variable,
        --   and witnesses to its properties.
        | LLetRegion  (Bind n) [Bind n]
        
        -- | Holds a region handle during evaluation.
        | LWithRegion (Bound n)
        deriving (Eq, Show)


-- | Describes how a let binding should be evaluated.
data LetMode n
        -- | Evaluate binding before substituting the result.
        = LetStrict

        -- | Use lazy evaluation. 
        --   The witness shows that the head region of the bound expression
        --   can contain thunks (is lazy), or Nothing if there is no head region.
        | LetLazy (Maybe (Witness n))
        deriving (Eq, Show)


-- | Case alternatives.
data Alt a n
        = AAlt (Pat n) (Exp a n)
        deriving (Eq, Show)


-- | Pattern matching.
data Pat n
        -- | The default pattern always succeeds.
        = PDefault
        
        -- | Match a data constructor and bind its arguments.
        --   TODO: the Bound here should really be a DaCon.
        --         It doesn't make sense to have UIx here.
        | PData (Bound n) [Bind n]
        deriving (Eq, Show)
        

-- Witness --------------------------------------------------------------------
-- | When a witness exists in the program it guarantees that a
--   certain property of the program is true.
data Witness n
        -- | Witness variable.
        = WVar  (Bound n)
        
        -- | Witness constructor.
        | WCon  (WiCon n)
        
        -- | Witness application.
        | WApp  (Witness n) (Witness n)

        -- | Joining of witnesses.
        | WJoin (Witness n) (Witness n)

        -- | Type can appear as the argument of an application.
        | WType (Type n)
        deriving (Eq, Show)


-- | Witness constructors.
data WiCon n
        -- | Witness constructors baked into the language.
        = WiConBuiltin WbCon

        -- | Witness constructors defined in the environment.
        --   In the interpreter we use this to hold runtime capabilities.
        --   The attached type must be closed.
        | WiConBound (Bound n) (Type n)
        deriving (Eq, Show)


-- | Built-in witness constructors.
--
--   These are used to convert a runtime capability into a witness that
--   the corresponding property is true.
data WbCon
        -- | (axiom) The pure effect is pure.
        -- 
        --   @pure     :: Pure !0@
        = WbConPure 

        -- | (axiom) The empty closure is empty.
        --
        --   @empty    :: Empty $0@
        | WbConEmpty

        -- | Convert a capability guaranteeing that a region is in the global
        --   heap into a witness that a closure using this region is empty.
        --   This lets us rely on the garbage collector to reclaim objects
        --   in the region. It is needed when we suspend the evaluation of 
        --   expressions that have a region in their closure, because the
        --   type of the returned thunk may not reveal that it references
        --   objects in that region.
        -- 
        --  @use      :: [r: %]. Global r => Empty (Use r)@
        | WbConUse      

        -- | Convert a capability guaranteeing the constancy of a region into
        --   a witness that a read from that region is pure.
        --   This lets us suspend applications that read constant objects,
        --   because it doesn't matter if the read is delayed, we'll always
        --   get the same result.
        --
        --   @read     :: [r: %]. Const r  => Pure (Read r)@
        | WbConRead     

        -- | Convert a capability guaranteeing the constancy of a region into
        --   a witness that allocation into that region is pure.
        --   This lets us increase the sharing of constant objects,
        --   because we can't tell constant objects of the same value apart.
        -- 
        --  @alloc    :: [r: %]. Const r  => Pure (Alloc r)@
        | WbConAlloc
        deriving (Eq, Show)

