
module DDC.Core.Exp 
        ( Exp  (..)
        , Cast (..)
        , Let  (..)
        , Alt  (..)
        , Pat  (..))
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

        -- | Type and witness application.
        | XAppT a (Exp a n p) (Type n)
        
        -- | Witness application.
        | XAppW a (Exp a n p) (Witness n)
        
        -- | Type cast.
        --   Argument is the witness for the cast.
        | XCast a (Exp a n p) (Type n)

        -- | Some possibly recursive definitions.
        | XLet  a (Let a n p) (Exp a n p)
                
        -- | Case branching.
        | XCase a (Exp a n p) [Alt a n p]
        deriving (Eq, Show)


-- | Type casts.
data Cast n
        -- | Purify the effect of an expression.
        = CastPurify  (Witness n)

        -- | Emptyfy the closure of an expression.
        | CastEmptify (Witness n)
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
        