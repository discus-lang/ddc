
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
        , WiCon   (..))
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

        -- | Recursively binding of lambda abstractions.
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
        | PData (Bound n) [Bind n]
        deriving (Eq, Show)
        

-- Witness --------------------------------------------------------------------
-- | When a witness exists in the program it guarantees that a
--   certain property of the program is true.
data Witness n
        -- | Witness variable.
        = WVar  (Bound n)
        
        -- | Witness constructor.
        | WCon  WiCon 
        
        -- | Witness application.
        | WApp  (Witness n) (Witness n)

        -- | Joining of witnesses.
        | WJoin (Witness n) (Witness n)

        -- | Type can appear as the argument of an application.
        | WType (Type n)
        deriving (Eq, Show)


-- | Witness constructors.
--
--   These are used to build proofs that certain properties are true.
--
--   The constructors marked (axiom) witness properties that are always true,
--   or combine existing witnesses in sound ways. 
--
--   The constructors marked (capability) are not normally present in the
--   initial program, but may be created by a `LLetRegion` during evaluation.
data WiCon
        -- | (axiom) The pure effect is pure.
        = WiConPure     -- pure     :: Pure (!0)

        -- | (axiom) The empty closure is empty.
        | WiConEmpty    -- empty    :: Empty ($0)

        -- | (axiom) Hide the use of a global region.
        --   This lets us empty the closure of an expression, and rely
        --   on the garbage collector to reclaim objects in that region.
        --   This is needed when we suspend function applications that 
        --   have such a region in their closure, because the type of the
        --   returned thunk doesn't reveal that it holds on to objects in 
        --   that region.
        | WiConUse      -- use      :: [r: %]. Global r => Empty (Use r)

        -- | (axiom) Purify a read effect from a constant region.
        --   This lets us suspend applications that read constant objects,
        --   because it doesn't matter if the read is delayed, we'll always
        --   ge the same result.
        | WiConRead     -- read     :: [r: %]. Const r  => Pure (Read r)

        -- | (axiom) Purify an allocation effect into a constant region.
        --   This lets us increase the sharing of constant objects,
        --   because we can't tell constant objects of the same value apart.
        | WiConAlloc    -- alloc    :: [r: %]. Const r  => Pure (Alloc r)

        -- | (capability) Witness that a region is global.
        --   Global regions live for the duration of the program and are not
        --   deallocated in a stack like manner. This lets us hide the use of
        --   such regions, and rely on the garbage collector to reclaim the
        --   space.
        | WiConGlobal   -- global   :: [r: %]. Global r

        -- | (capability) Witness that a region is constant.
        --   This lets us purify read and allocation effects on it,
        --   and prevents it from being Mutable.
        | WiConConst    -- const    :: [r: %]. Const r
        
        -- | (capability) Witness that a region is mutable.
        --   This lets us update objects in the region, 
        --   and prevents it from being Constant.
        | WiConMutable  -- mutable  :: [r: %]. Mutable r

        -- | (capability) Witness that a region is lazy.
        --   This lets is allocate thunks into the region,
        --   and prevents it from being Manifest.
        | WiConLazy     -- lazy     :: [r: %].Lazy r
        
        -- | (capability) Witness that a region is manifest.
        --   This ensures there are no thunks in the region,
        --   which prevents it from being Lazy.
        | WiConManifest -- manifest :: [r: %]. Manifest r

        deriving (Eq, Show)

