
module DDC.Core.Exp
        ( Bound(..)
        , Bind (..)
        
        -- * Kind, type and witness expressions.
        , Type (..)
        , TCon (..)
        , KiCon(..)
        , TyCon(..)
        , WiCon(..)

        -- * Value expressions.
        , Exp  (..)
        , XCon (..)
        , Alt  (..)
        , Let  (..)
        , Stmt (..))
where


-- Variables --------------------------------------------------------------------------------------
-- | Bound occurrence of a variable.
data Bound v
        -- | Bound variable, with its type.
        = UVar  v (Type v)

        -- | Bound varaible with its type and more-than constraint.
        | UMore v (Type v) (Type v)
        deriving (Eq, Show)
        

-- | Binding occurrence of a variable.
data Bind v
        -- | Binding variable, with its type.
        = BVar  v (Type v)
        
        -- | Binding variable with its type and more-than constraint.
        | BMore v (Type v) (Type v)
        deriving (Eq, Show)


-- Types ------------------------------------------------------------------------------------------
-- | A kind, type, or witness. The domain of proof.
--   We use the same data type to represent all three, because they have a similar structure.
data Type v
        -- | Type constructor.
        = TCon  (TCon  v)

        -- | Type variable.
        | TVar  (Bound v)
        
        -- | Type abstraction.
        | TLam  (Bind  v)
        
        -- | Type application.
        | TApp  (Type  v) (Type v)
        
        -- | Least upper bound.
        | TSum  (Type  v) (Type v)
        
        -- | Joining of witnesses.
        | TJoin (Type v)  (Type v)
        
        -- | Separation of regions.
        | TSep  (Type v)  (Type v)
        deriving (Eq, Show)


-- | Kind, type and witness constructors.
data TCon v
        -- | Kind constructor.
        = TConKind KiCon

        -- | Type constructor.
        | TConType (TyCon v)

        -- | Witness constructor.
        | TConWit  (WiCon v)
        deriving (Eq, Show)


-- | Kind constructor.
data KiCon
        -- | Kind of everything
        = KiBox         Int                     -- **

        -- | Function kind.
        | KiConFun      KiCon KiCon

        -- | Kind of values.
        | KiConValue                            -- *

        -- | Kind of regions.
        | KiConRegion                           -- %

        -- | Kind of effects.
        | KiConEffect                           -- !

        -- | Kind of closures.
        | KiConClosure                          -- $

        -- | Kind of witnesses.
        | KiConWitness                          -- @
        deriving (Eq, Show)


-- | Type constructor.
data TyCon v
        -- | The function type constructor.
        = TyConFun
        
        -- | Read of some region
        | TyConRead

        -- | Read of all regions in some type
        | TyConDeepRead
        
        -- | Write of some region
        | TyConWrite
        
        -- | Write to all regions in some type
        | TyConDeepWrite
        
        -- | Allocation into some region
        | TyConAlloc
        
        -- | Some region is free in a closure.
        | TyConFree
        
        -- | All material variables in a type are free in a closure.
        | TyConDeepFree
        
        -- | User data constructor with its type.
        | TyConData v (Type v)
        deriving (Eq, Show)


-- | Witness constructor.
data WiCon v
        -- | Constancy of some region.
        = WiConConst            -- :: % -> @

        -- | Constancy of all regions in some type
        | WiConDeepConst        -- :: * -> @

        -- | Mutability of some region.
        | WiConMutable          -- :: % -> @

        -- | Mutability of all regions in some type.
        | WiConDeepMutable      -- :: * -> @

        -- | Laziness of some region.
        | WiConLazy             -- :: % -> @

        -- | Laziness of the primary region in some type.
        | WiConHeadLazy         -- :: * -> @

        -- | Directness of some region (not lazy).
        | WiConDirect           -- :: % -> @

        -- | Purity of some effect.
        --     Pure e1  <> Pure e2  = Pure (e1 + e2)
        | WiConPure             -- :: ! -> @

        -- | Emptiness of some closure.
        --     Empty c1 <> Empty c2 = Empty (c1 + c2)
        | WiConEmpty            -- :: $ -> @
        
        -- | Purity a read from a region.
        | WiConPurify           -- :: Const r -> Pure (Read r)
                
        -- | Witness to some user defined type class constraint.
        | WiConClass v (Type v)
        deriving (Eq, Show)


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
        | XAPP  n (Exp n v p) (Type v)
        
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

        -- | Local region binding with witnesses to its properties.
        | XLocal  v [(v, Type v)] (Exp n v p)
        deriving (Eq, Show)


-- | Statement, perhaps binding a variable.
data Stmt n v p
        = SStmt (Maybe (Bind v)) (Exp n v p)
        deriving (Eq, Show)


data Cast
        -- | Weakening some effect
        = CastWeakenEffect  (Type v)

        -- | Weakening some closure
        | CastWeakenClosure (Type v)

        -- | Purify an effect.
        | CastPurify        (Type v)
        deriving (Eq, Show)
