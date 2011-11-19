
module DDC.Type.Exp
        -- * Types, Kinds, and Sorts
        ( Type    (..)
        , Bind    (..)
        , Bound   (..)
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

-- Types ------------------------------------------------------------------------------------------
-- | A value type (level 1), kind (level 2) or sort (level 3).
--   We use the same data type to represent all three, as they have a similar structure.
data Type v c
        -- | Type constructor.
        = TVar    (Bound v c)

        | TCon    (TCon  v c)

        -- | Type abstraction.
        | TForall (Bind  v c) (Type  v c)
        
        -- | Type application.
        | TApp    (Type  v c) (Type  v c)

        -- | Least upper bound.
        --   TODO: change this to be a type sum.
        --         use an array of maps, where the key is the tycon.
        --         Find just to support single arity type constructors.
        --         Make a hash function to convert summable TyCons to ints for the array.
        | TSum    (Type v c) (Type v c)

        -- | Least element of some kind.
        --   Parameters at the next level up.
        | TBot    (Kind v c)
        deriving (Eq, Show)


type Sort    v c = Type v c
type Kind    v c = Type v c
type Region  v c = Type v c
type Effect  v c = Type v c
type Closure v c = Type v c


-- Bind -------------------------------------------------------------------------------------------
-- | Binding occurrence of a variable.
data Bind v c
        = BName v   (Kind v c)
        | BAnon     (Kind v c)
        deriving (Eq, Show)
        

-- | Bound occurrence of a variable.
--   If the varibles haven't been annotated with their kinds then the kind  field will be TBot. 
data Bound v c
        = UName v   (Kind v c)
        | UIx   Int (Kind v c)
        deriving (Eq, Show)


-- TCon -------------------------------------------------------------------------------------------
-- | Kind, type and witness constructors.
data TCon v c
        -- | Sort constructor  (level 3)
        = TConSort    SoCon

        -- | Kind constructors (level 2)
        --   The kind function is treated separtely because it isn't well formed
        --   without being fully applied.
        | TConKindFun 
        | TConKind    KiCon

        -- | Type constructor  (level 1)
        | TConType    (TyCon v c)
        deriving (Eq, Show)


-- | Sort constructor.
data SoCon
        -- | Sort of computation kinds.
        = SoConComp                -- '**'

        -- | Sort of proof kinds.
        | SoConProp                -- '@@'
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
data TyCon v c

        -- Value type constructors --------------
        -- | User data constructor with its type.
        = TyConData c (Kind v c)

        -- | The function type constructor.
        | TyConFun              -- '(->) :: * ~> * ~> ! ~> $ ~> *'
        

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
data Witness v
        -- | Witness constructor.
        = WCon  WiCon 
        
        -- | Witness variable.
        | WVar  v
        
        -- | Witness application.
        | WApp  (Witness v) (Witness v)

        -- | Joining of witnesses.
        | WJoin (Witness v) (Witness v)
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
