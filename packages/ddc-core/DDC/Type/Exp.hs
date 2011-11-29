
module DDC.Type.Exp
        ( -- * Types, Kinds, and Sorts
          Type     (..)
        , Bind     (..)
        , Bound    (..)
        , Kind,   Sort
        , Region, Effect, Closure
        , TypeSum  (..),   TyConHash(..)
        , TyCon    (..)
        , SoCon    (..)
        , KiCon    (..)
        , TwCon    (..)
        , TcCon    (..))
where
import Data.Array
import Data.Map         (Map)


-- Types ------------------------------------------------------------------------------------------
-- | A value type, kind, or sort.
--
--   We use the same data type to represent all three universes, as they have a similar
--   algebraic structure.
--
data Type n
        -- | Variable.
        = TVar    (Bound n)

        -- | Constructor.
        | TCon    (TyCon n)

        -- | Abstraction.
        | TForall (Bind  n) (Type  n)
        
        -- | Application.
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
-- 
--   * If the variables haven't been annotated with their kinds then the kind field will be TBot. 
data Bound n
        = UName n   (Kind n)
        | UIx   Int (Kind n)
        deriving (Eq, Show)


-- Type Sums --------------------------------------------------------------------------------------
-- | A least upper bound of several types.
-- 
--   We keep type sums in this normalised format instead of joining them together with the binary
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
        -- TODO: Eq instance is wrong because we much check the spill fields.
        
-- | Hash value used to insert types into type sums.
--   Only tycons that can be inserted into the sum have a hash value.
data TyConHash 
        = TyConHash !Int
        deriving (Eq, Show, Ord, Ix)


-- TyCon ------------------------------------------------------------------------------------------
-- | Kind, type and witness constructors.
--
--   These are grouped to make it easy to determine the universe that they belong to.
-- 
data TyCon n
        -- | Sort constructors               (level 3)
        = TyConSort    SoCon

        -- | Kind constructors               (level 2)
        | TyConKind    KiCon

        -- | Witness type constructors       (level 1)
        | TyConWitness TwCon

        -- | Computation type constructors   (level 1)
        | TyConComp    (TcCon n)
        deriving (Eq, Show)


-- | Sort constructor.
data SoCon
        -- | Sort of witness kinds.
        = SoConProp                -- '@@'

        -- | Sort of computation kinds.
        | SoConComp                -- '**'
        deriving (Eq, Show)


-- | Kind constructor.
data KiCon
        -- | Function kind constructor.
        --   This is only well formed, and has a sort, when it is fully applied.
        = KiConFun              -- (~>)

        -- Witness kinds ------------------------
        -- | Kind of witnesses.
        | KiConWitness          -- '@ :: @@'

        -- Computation kinds ---------------------
        -- | Kind of data values.
        | KiConData             -- '* :: **'

        -- | Kind of regions.
        | KiConRegion           -- '% :: **'

        -- | Kind of effects.
        | KiConEffect           -- '! :: **'

        -- | Kind of closures.
        | KiConClosure          -- '$ :: **'
        deriving (Eq, Show)


-- | Witness type constructors.
data TwCon
        -- Witness implication.
        = TwConImpl             -- :: '(=>) :: * ~> *'
        
        -- | Constancy of some region.
        | TwConConst            -- :: % ~> @

        -- | Constancy of material regions in some type
        | TwConDeepConst        -- :: * ~> @

        -- | Mutability of some region.
        | TwConMutable          -- :: % ~> @

        -- | Mutability of material regions in some type.
        | TwConDeepMutable      -- :: * ~> @

        -- | Laziness of some region.
        | TwConLazy             -- :: % ~> @

        -- | Laziness of the primary region in some type.
        | TwConHeadLazy         -- :: * ~> @

        -- | Directness of some region (not lazy).
        | TwConDirect           -- :: % ~> @

        -- | Distinctness \/ Separation of regions.
        --   Arity must be >= 2.
        | TwConDistinct Int     -- :: % ~> % ... ~> @

        -- | Purity of some effect.
        | TwConPure             -- :: ! ~> @

        -- | Emptiness of some closure.
        | TwConEmpty            -- :: $ ~> @
        deriving (Eq, Show)


-- | Computation type constructors.
data TcCon n
        -- Data type constructors ---------------
        -- | Data type constructor with its kind.
        = TcConData     n (Kind n)

        -- | The function type constructor is baked in so we 
        --   represent it separately.
        | TcConFun              -- '(->) :: * ~> * ~> ! ~> $ ~> *'


        -- Effect type constructors -------------
        -- | Read of some region
        | TcConRead             -- :: '% ~> !'

        -- | Read of all material regions in value type.
        | TcConDeepRead         -- :: '* ~> !'
        
        -- | Write of some region.
        | TcConWrite            -- :: '% ~> !'

        -- | Write to all material regions in some type
        | TcConDeepWrite        -- :: '* ~> !'
        
        -- | Allocation into some region.
        | TcConAlloc            -- :: '% ~> !'

        
        -- Closure type constructors ------------
        -- | Some region is free in a closure.
        | TcConFree             -- :: '% ~> $'
        
        -- | All material regions in a type are free in a closure.
        | TcConDeepFree         -- :: '* ~> $'
        deriving (Eq, Show)

