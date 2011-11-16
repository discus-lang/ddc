
module DDC.Core.Exp 
        ( -- * Value expressions.
          Exp  (..)
        , XCon (..)
        , Alt  (..)
        , Let  (..)
        , Stmt (..))
where
import DDC.Type.Exp


-- Values -----------------------------------------------------------------------------------------
-- | A value expression,
--   The domain of computation.
data Exp n v p
        -- | A primitive operator or literal.
        = XPrim n p

        -- | Data constructor.
        | XCon  n (XCon v)

        -- | Value variable.
        | XVar  n (Bound v)
        
        -- | Function abstraction.
        | XLam  n (Bind v)    (Exp n v p)
        
        -- | Value application.
        | XApp  n (Exp n v p) (Exp n v p)

        -- | Type and witness application.
        | XTApp n (Exp n v p) (Type v)
        
        -- | Witness application.
        | XWApp n (Exp n v p) (Witness v)
        
        -- | Type cast.
        --   Argument is the witness for the cast.
        | XCast n (Exp n v p) (Type v)
                
        -- | Case branching.
        | XCase n (Exp n v p) [Alt n v p]
        
        -- | Some recursive definitions.
        | XRec  n [Let n v p] (Exp n v p)
        
        -- | Some non-recursive statements.
        | XDo   n [Stmt n v p]
        deriving (Eq, Show)


-- | Data Constructors
data XCon v
        = XConData v
        deriving (Eq, Show)


-- | Case alternatives
data Alt n v p
        = XAlt (XCon v) [Bind v] (Exp n v p)
        deriving (Eq, Show)


-- | Possibly recursive let bindings
data Let n v p
        -- | Possibly recursive function binding.
        = XLet    (Bind v)        (Exp n v p)

        -- | Create a local region binding with witnesses to its properties.
        | XLocal v (Type v) [(v, Type v)] (Exp n v p)
        deriving (Eq, Show)


-- | Statement, perhaps binding a variable.
data Stmt n v p
        = SStmt (Maybe (Bind v)) (Exp n v p)
        deriving (Eq, Show)


data Cast v
        -- | Weakening some effect
        = CastWeakenEffect  (Type v)

        -- | Weakening some closure
        | CastWeakenClosure (Type v)

        -- | Purify an effect.
        | CastPurify        (Type v)
        deriving (Eq, Show)
