
module DDC.Type.Exp
        ( NameDef (..)
        , NameUse (..)
        , Bound   (..)
        , Bind    (..)
        
        -- * Types, Kinds, and Sorts
        , Type    (..)
        , Kind,   Sort
        , Region, Effect, Closure
        , TCon    (..)
        , SoCon   (..)
        , KiCon   (..)
        , TyCon   (..)
        
        -- * Witness.
        , Witness (..)
        , WiCon   (..))
where

-- Name -------------------------------------------------------------------------------------------
-- | Defining occurrence of a name.
--   We support both absolute textual names and relative deBruijn names.
data NameDef n
        -- | A textual name.
        = NDName n
        | NDAnon
        deriving (Eq, Show)


-- | Use of a name.
data NameUse n
        = NUName n
        -- | The bound occurrence of a deBruijn name.
        | NUIx   Int
        deriving (Eq, Show)


-- Binds and Bounds -------------------------------------------------------------------------------
-- TODO: refactor these into single constructors with a Maybe for the constraint.
-- | Binding occurrence of a variable.
data Bind n
        -- | Binding variable, with its type.
        = BVar  (NameDef n) (Type n)
        
        -- | Binding variable with its type and more-than constraint.
        | BMore (NameDef n) (Type n) (Type n)
        deriving (Eq, Show)


-- | Bound occurrence of a variable.
data Bound n
        -- | Bound variable, with its type.
        = UVar   (NameUse n) (Type n)

        -- | Bound varaible with its type and more-than constraint.
        | UMore  (NameUse n) (Type n) (Type n)
        deriving (Eq, Show)
        

-- Types ------------------------------------------------------------------------------------------
-- | A value type (level 1), kind (level 2) or sort (level 3).
--   We use the same data type to represent all three, as they have a similar structure.
data Type n
        -- | Type constructor.
        = TCon    (TCon  n)

        -- | Type variable.
        | TVar    (Bound n)
        
        -- | Type abstraction.
        | TForall (Bind n) (Type n)
        
        -- | Type application.
        | TApp    (Type n) (Type n)

        -- | Least upper bound.
        --   TODO: change this to be a type sum.
        --         use an array of maps, where the key is the tycon.
        --         Find just to support single arity type constructors.
        --         Make a hash function to convert summable TyCons to ints for the array.
        | TSum    (Type n) (Type n)

        -- | Least element of some kind.
        --   Parameters at the next level up.
        | TBot    (Type n)
        deriving (Eq, Show)

type Sort n     = Type n
type Kind n     = Type n
type Region n   = Type n
type Effect n   = Type n
type Closure n  = Type n


-- | Kind, type and witness constructors.
data TCon n
        -- | Sort constructor  (level 3)
        = TConSort    SoCon

        -- | Kind constructors (level 2)
        --   The kind function is treated separtely because it isn't well formed
        --   without being fully applied.
        | TConKindFun 
        | TConKind    KiCon

        -- | Type constructor  (level 1)
        | TConType    (TyCon n)
        deriving (Eq, Show)


-- | Sort constructor.
data SoCon
        -- | Sort of computation kinds.
        = SoComp                -- '**'

        -- | Sort of proof kinds.
        | SoProp                -- '@@'
        deriving (Eq, Show)


-- | Kind constructor.
data KiCon
        -- | Kind of data values.
        = KiConData             -- '* :: **'

        -- | Kind of regions.
        | KiConRegion           -- '% :: **'

        -- | Kind of effects.
        | KiConEffect           -- '! :: **'

        -- | Kind of closures.
        | KiConClosure          -- '$ :: **'

        -- | Kind of witnesses.
        | KiConWitness          -- '@ :: ++'
        deriving (Eq, Show)


-- | Type constructor.
data TyCon n

        -- Value type constructors --------------
        -- | The function type constructor.
        = TyConFun              -- '(->) :: * ~> * ~> ! ~> $ ~> *'

        -- | User data constructor with its type.
        | TyConData n (Kind n)

        
        -- Effect type constructors -------------
        -- | Read of some region
        | TyConRead             -- :: '% ~> !'

        -- | Read of all material regions in value type.
        | TyConDeepRead         -- :: '* ~> !'
        
        -- | Write of some region.
        | TyConWrite            -- :: '% ~> !'

        -- | Write to all material regions in some type
        | TyConDeepWrite        -- :: '* ~> !'
        
        -- | Allocation into some region.
        | TyConAlloc            -- :: '% ~> !'

        
        -- Closure type constructors ------------
        -- | Some region is free in a closure.
        | TyConFree             -- :: '% ~> $'
        
        -- | All material regions in a type are free in a closure.
        | TyConDeepFree         -- :: '* ~> $'


        -- Witness type constructors ------------
        -- Witness implication.
        | TyConImpl             -- :: '(=>)'
        
        -- | Constancy of some region.
        | TyConConst            -- :: % ~> @

        -- | Constancy of material regions in some type
        | TyConDeepConst        -- :: * ~> @

        -- | Mutability of some region.
        | TyConMutable          -- :: % ~> @

        -- | Mutability of material regions in some type.
        | TyConDeepMutable      -- :: * ~> @

        -- | Laziness of some region.
        | TyConLazy             -- :: % ~> @

        -- | Laziness of the primary region in some type.
        | TyConHeadLazy         -- :: * ~> @

        -- | Directness of some region (not lazy).
        | TyConDirect           -- :: % ~> @

        -- | Distinctness \/ Separation of regions.
        --   Arity must be >= 2.
        | TyConDistinct Int     -- :: % ~> % ... ~> @

        -- | Purity of some effect.
        | TyConPure             -- :: ! ~> @

        -- | Emptiness of some closure.
        | TyConEmpty            -- :: $ ~> @
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
        deriving (Eq, Show)


-- | Witness constructor.
data WiCon
        -- | The pure effect is pure
        = WiConMkPure           -- :: Pure (!0)

        -- | The empty closure is empty
        | WiConMkEmpty          -- :: Empty ($0)

        -- | Witness that a region is constant.
        | WiConMkConst          -- :: \(r: %). Const r
        
        -- | Witness that a region is mutable.
        | WiConMkMutable        -- :: \(r: %). Mutable r

        -- | Witness that a region is lazy.
        | WiConMkLazy           -- :: \(r: %). Const r
        
        -- | Witness that a region is direct.
        | WiConMkDirect         -- :: \(r: %). Mutable r

        -- | Purify a read from a constant region.
        | WiConMkPurify         -- :: \(r: %). Const r => Pure  (Read r)

        -- | Hide the sharing of a constant region.
        | WiConMkShare          -- :: \(r: %). Const r => Empty (Free r)

        -- | Witness that some regions are distinct.
        | WiConMkDistinct Int   -- :: \(r0 r1 ... rn : %). Distinct_n r0 r1 .. rn
        deriving (Eq, Show)
