
-- | Abstract syntax for the DDC core language.
module DDC.Core.Exp 
        ( module DDC.Type.Exp

          -- * Computation expressions
        , Exp  (..)
        , Cast (..)
        , Lets (..)
        , Alt  (..)
        , Pat  (..)
                        
          -- * Witnesses expressions
        , Witness (..)
        , WiCon   (..))
where
import DDC.Type.Exp


-- Values -----------------------------------------------------------------------------------------
-- | A value expression, universe of computation.
data Exp a n
        -- | Value variable   or primop.
        = XVar  a  (Bound n)

        -- | Data constructor or literal.
        | XCon  a  (Bound n)

        -- | Value application.
        | XApp  a  (Exp a n)  (Exp a n)

        -- | Function abstraction.
        | XLam  a  (Bind n)   (Exp a n)

        -- | Some possibly recursive definitions.
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

        -- | Hide sharing in a closure of an expression.
        | CastForget (Witness n)
        deriving (Eq, Show)


-- | Possibly recursive bindings.
data Lets a n
        -- | Non-recursive binding
        = LLet    (Bind n) (Exp a n)
        
        -- | Recursive binding
        | LRec    [(Bind n, Exp a n)]

        -- | Bind a local region variable, and (non-recursive) witnesses to its properties.
        | LLetRegion  (Bind n) [Bind n]
        
        -- | Holds a region handle during evaluation.
        | LWithRegion (Bound n)
        deriving (Eq, Show)


-- | Case alternatives.
data Alt a n
        = XAlt (Pat n) (Exp a n)
        deriving (Eq, Show)


-- | Pattern matches.
data Pat n
        -- | The default pattern always succeeds.
        = PDefault
        
        -- | Match a data constructor and bind its arguments.
        | PData (Bound n) [Bind n]
        deriving (Eq, Show)
        

-- Witness ----------------------------------------------------------------------------------------
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


-- | Witness constructor.
data WiCon
        -- | The pure effect is pure.
        = WiConPure     -- pure     :: Pure (!0)

        -- | The empty closure is empty.
        | WiConEmpty    -- empty    :: Empty ($0)

        -- | Witness that a region is global.
        --   Global regions live for the duration of the program and are not
        --   deallocated in a stack like manner. This lets us hide the use of
        --   such regions, and rely on the garbage collector to reclaim the
        --   space.
        | WiConGlobal   -- global   :: [r: %]. Global r

        -- | Witness that a region is constant.
        --   This lets us purify read and allocation effects on it,
        --   and prevents it from being Mutable.
        | WiConConst    -- const    :: [r: %]. Const r
        
        -- | Witness that a region is mutable.
        --   This lets us update objects in the region, 
        --   and prevents it from being Constant.
        | WiConMutable  -- mutable  :: [r: %]. Mutable r

        -- | Witness that a region is lazy.
        --   This lets is allocate thunks into the region,
        --   and prevents it from being Direct.
        | WiConLazy     -- lazy     :: [r: %]. Const r
        
        -- | Witness that a region is direct.
        --   This ensures there are no thunks in the region,
        --   which prevents it from being Lazy.
        | WiConDirect   -- direct   :: [r: %]. Mutable r

        -- | Hide the use of a global region.
        --   This lets us empty the closure of an expression, and rely
        --   on the garbage collector to reclaim objects in that region.
        --   This is needed when we suspend function applications that 
        --   have such a region in their closure, because the type of the
        --   returned thunk doesn't reveal that it holds on to objects in 
        --   that region.
        | WiConUse      -- use      :: [r: %]. Global r => Const r => Empty (Use r)

        -- | Purify a read effect from a constant region.
        --   This lets us suspend applications that read constant objects,
        --   because it doesn't matter if the read is delayed, we'll always
        --   ge the same result.
        | WiConRead     -- read     :: [r: %]. Const r => Pure (Read r)

        -- | Purify an allocation effect into a constant region.
        --   This lets us increase the sharing of constant objects,
        --   because we can't tell constant objects of the same value apart.
        | WiConAlloc    -- alloc    :: [r: %]. Const r => Pure (Alloc r)

        -- | Witness that some regions are distinct.
        --   This ensures that the regions are physically seprate, so reads
        --   and writes into one won't interfere with reads and writes
        --   to the other. We have a family of constructors of arbitray arity,
        --   that encode that number of regions all being distinct from each 
        --   other.
        | WiConDistinct Int     -- distinct :: [r0 r1 ... rn : %]. Distinct_n r0 r1 .. rn
        deriving (Eq, Show)

