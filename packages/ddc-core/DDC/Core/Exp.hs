
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
        | XCast a  (Exp a n)  (Cast n)

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
        -- | Witness constructor.
        = WCon  WiCon 
        
        -- | Witness variable.
        | WVar  (Bound n)
        
        -- | Witness application.
        | WApp  (Witness n) (Witness n)

        -- | Joining of witnesses.
        | WJoin (Witness n) (Witness n)

        -- | Type can appear as the argument of an application.
        | WType (Type n)
        deriving (Eq, Show)


-- | Witness constructor.
data WiCon
        -- | The pure effect is pure
        = WiConPure             -- pure     :: Pure (!0)

        -- | The empty closure is empty
        | WiConEmpty            -- empty    :: Empty ($0)

        -- | Witness that a region is constant.
        | WiConConst            -- const    :: [r: %]. Const r
        
        -- | Witness that a region is mutable.
        | WiConMutable          -- mutable  :: [r: %]. Mutable r

        -- | Witness that a region is lazy.
        | WiConLazy             -- lazy     :: [r: %]. Const r
        
        -- | Witness that a region is direct.
        | WiConDirect           -- direct   :: [r: %]. Mutable r

        -- | Purify a read from a constant region.
        | WiConRead             -- read     :: [r: %]. Const r => Pure  (Read r)

        -- | Hide the sharing of a constant region.
        | WiConShare            -- share    :: [r: %]. Const r => Empty (Share r)

        -- | Witness that some regions are distinct.
        | WiConDistinct Int     -- distinct :: [r0 r1 ... rn : %]. Distinct_n r0 r1 .. rn
        deriving (Eq, Show)

