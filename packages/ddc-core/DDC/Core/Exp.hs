
module DDC.Core.Exp 
        ( module DDC.Type.Exp

          -- * Value expressions.
        , Exp  (..)
        , Cast (..)
        , Let  (..)
        , Alt  (..)
        , Pat  (..)
                        
          -- * Witnesses expressions.
        , Witness (..)
        , WiCon   (..))
where
import DDC.Type.Exp


-- Values -----------------------------------------------------------------------------------------
-- | A value expression,
--   The domain of computation.
data Exp a n p
        -- | A primitive operator or literal.
        = XPrim a p

        -- | Value variable.
        | XVar  a (Bound n)

        -- | Data constructor.
        | XCon  a (Bound n)
        
        -- | Function abstraction.
        | XLam  a (Bind n)    (Exp a n p)
        
        -- | Value application.
        | XApp  a (Exp a n p) (Exp a n p)

        -- | Type application.
        | XAppT a (Exp a n p) (Type n)
        
        -- | Witness application.
        | XAppW a (Exp a n p) (Witness n)
        
        -- | Type cast.
        --   Argument is the witness for the cast.
        | XCast a (Exp a n p) (Cast n)

        -- | Some possibly recursive definitions.
        | XLet  a (Let a n p) (Exp a n p)
                
        -- | Case branching.
        | XCase a (Exp a n p) [Alt a n p]
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
data Let a n p
        -- | A non-binding, effectful statement.
        = LStmt          (Exp a n p)
        
        -- | Non-recursive binding
        | LLet  (Bind n) (Exp a n p)
        
        -- | Recursive binding
        | LRec  [(Bind n, Exp a n p)]

        -- | Bind a local region variable, and (non-recursive) witnesses to its properties.
        | XLocal (Bind n) [(Bind n, Type n)]
        deriving (Eq, Show)


-- | Case alternatives.
data Alt a n p
        = XAlt (Pat n p) (Exp a n p)
        deriving (Eq, Show)


-- | Pattern matches.
data Pat n p

        -- | The default pattern always succeeds.
        = PDefault

        -- | Match a literal.
        | PLit  p

        -- | Match a data constructor and bind its arguments.
        | PData (Bound n) [Bind n]
        deriving (Eq, Show)
        

-- Witness ----------------------------------------------------------------------------------------
data Witness n
        -- | Witness constructor.
        = WCon  WiCon 
        
        -- | Witness variable.
        | WVar  n
        
        -- | Witness application.
        | WApp  (Witness n) (Witness n)

        -- | Joining of witnesses.
        | WJoin (Witness n) (Witness n)
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
        | WiConFree             -- free     :: [r: %]. Const r => Shared r => Empty (Free r)

        -- | Witness that some regions are distinct.
        | WiConDistinct Int     -- distinct :: [r0 r1 ... rn : %]. Distinct_n r0 r1 .. rn
        deriving (Eq, Show)

        