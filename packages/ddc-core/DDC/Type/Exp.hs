
module DDC.Type.Exp
        -- * Types, Kinds, and Sorts
        ( Type    (..)
        , Bind    (..)
        , Bound   (..)
        , Kind,   Sort
        , Region, Effect, Closure
        , TypeSum (..),   TyConHash(..)
        , TCon    (..)
        , SoCon   (..)
        , KiCon   (..)
        , TyCon   (..),   TyConBuiltin(..)
        
        -- * Witness.
        , Witness (..)
        , WiCon   (..))
where
import Data.Array
import Data.Map         (Map)


-- Types ------------------------------------------------------------------------------------------
-- | A value type (level 1), kind (level 2) or sort (level 3).
--   We use the same data type to represent all three, as they have a similar structure.
data Type n
        -- | Type variable.
        = TVar    (Bound n)

        -- | Type constructor.
        | TCon    (TCon  n)

        -- | Type abstraction.
        | TForall (Bind  n) (Type  n)
        
        -- | Type application.
        | TApp    (Type  n) (Type  n)

        -- | Least upper bound.
        | TSum    (TypeSum n)

        -- | Least element of some kind.
        | TBot    (Kind n)
        deriving (Eq, Show)


type Sort    n = Type n
type Kind    n = Type n
type Region  n = Type n
type Effect  n = Type n
type Closure n = Type n


-- Bind -------------------------------------------------------------------------------------------
-- | Binding occurrence of a variable.
data Bind n
        = BName n   (Kind n)
        | BAnon     (Kind n)
        deriving (Eq, Show)
        

-- | Bound occurrence of a variable.
--   If the varibles haven't been annotated with their kinds then the kind  field will be TBot. 
data Bound n
        = UName n   (Kind n)
        | UIx   Int (Kind n)
        deriving (Eq, Show)


-- Type Sums --------------------------------------------------------------------------------------
-- | We keep type sums in a normalised format instead of joining them together with the binary
--   operator (+). This makes them much easier to work with, as a given sum type often only has
--   a single physical representation.
data TypeSum n
        = TypeSum
        { -- | The kind of all the elements in this sum.
          typeSumKind           :: Kind n

          -- | Where we can see the outer constructor of a type its argument is inserted into this 
          --   array. This handles most common cases like Read, Write, Alloc effects.
        , typeSumElems          :: Array TyConHash [Type n]

          -- | A map for named type variables.
        , typeSumBoundNamed     :: Map n   (Kind n)

          -- | A map for anonymous type variables.
        , typeSumBoundAnon      :: Map Int (Kind n)

          -- | Types that can't be placed in the other fields go here.
          --   INVARIANT: this list doesn't contain other TSum forms.
        , typeSumSpill          :: [Type n] }
        deriving (Eq, Show)


-- | Hash value used to insert types into type sums.
--   Only tycons that can be inserted into the sum have a hash value.
data TyConHash 
        = TyConHash !Int
        deriving (Eq, Show, Ord, Ix)


-- TCon -------------------------------------------------------------------------------------------
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
data TyCon n
        -- | User defined type constructor with its kind.
        = TyConUser     n (Kind n)

        -- | A builtin type constructor.
        | TyConBuiltin  TyConBuiltin
        deriving (Eq, Show)


-- | Builtin type constructors that don't contain names.
data TyConBuiltin
        -- Value type constructors --------------
        -- | The function type constructor.
        = TyConFun              -- '(->) :: * ~> * ~> ! ~> $ ~> *'


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
        | WVar  n
        
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
        | WiConMkConst          -- :: [r: %]. Const r
        
        -- | Witness that a region is mutable.
        | WiConMkMutable        -- :: [r: %]. Mutable r

        -- | Witness that a region is lazy.
        | WiConMkLazy           -- :: [r: %]. Const r
        
        -- | Witness that a region is direct.
        | WiConMkDirect         -- :: [r: %]. Mutable r

        -- | Purify a read from a constant region.
        | WiConMkPurify         -- :: [r: %]. Const r => Pure  (Read r)

        -- | Hide the sharing of a constant region.
        | WiConMkShare          -- :: [r: %]. Const r => Empty (Free r)

        -- | Witness that some regions are distinct.
        | WiConMkDistinct Int   -- :: [r0 r1 ... rn : %]. Distinct_n r0 r1 .. rn
        deriving (Eq, Show)
