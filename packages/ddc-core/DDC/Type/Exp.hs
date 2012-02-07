
module DDC.Type.Exp
        ( -- * Types, Kinds, and Sorts
          Type     (..)
        , Binder  (..)
        , Bind     (..)
        , Bound    (..)
        , Kind,   Sort
        , Region, Effect, Closure
        , TypeSum  (..),   TyConHash(..), TypeSumVarCon(..)
        , TyCon    (..)
        , SoCon    (..)
        , KiCon    (..)
        , TwCon    (..)
        , TcCon    (..))
where
import Data.Array
import Data.Map         (Map)
import Data.Set         (Set)


-- Types ----------------------------------------------------------------------
-- | A value type, kind, or sort.
--
--   We use the same data type to represent all three universes, as they have
--  a similar algebraic structure.
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
        deriving Show


type Sort    n = Type n
type Kind    n = Type n
type Region  n = Type n
type Effect  n = Type n
type Closure n = Type n


-- Bind -----------------------------------------------------------------------
-- | Binding occurrence of a variable.
data Bind n
        -- | A variable with no uses in the body doesn't need a name.
        = BNone     (Type n)

        -- | Nameless variable on the deBruijn stack.
        | BAnon     (Type n)

        -- | Named variable in the environment.
        | BName n   (Type n)
        deriving Show


-- | Represents the binder of a `Bind`, without the type.
data Binder n
        = RNone
        | RAnon
        | RName n
        deriving Show


-- | Bound occurrence of a variable.
-- 
--   * If the variables haven't been annotated with their kinds then the kind
--     field will be TBot. 
--
data Bound n
        -- | Nameless variable that should be on the deBruijn stack.
        = UIx   Int (Type n)    

        -- | Named variable that should be in the environment.
        | UName n   (Type n)

        -- | Named primitive that is not bound in the environment.
        --   Prims aren't every counted as being free.
        | UPrim n   (Type n)    
        deriving Show


-- Type Sums ------------------------------------------------------------------
-- | A least upper bound of several types.
-- 
--   We keep type sums in this normalised format instead of joining them
--   together with a binary operator (+). This makes them easier to work with,
--   as a given sum type often only has a single physical representation.
data TypeSum n
        = TypeSum
        { -- | The kind of all the elements in this sum.
          typeSumKind           :: Kind n

          -- | Where we can see the outer constructor of a type its argument
          --   is inserted into this array. This handles common cases like
          --   Read, Write, Alloc effects.
        , typeSumElems          :: Array TyConHash (Set (TypeSumVarCon n))

          -- | A map for named type variables.
        , typeSumBoundNamed     :: Map n   (Kind n)

          -- | A map for anonymous type variables.
        , typeSumBoundAnon      :: Map Int (Kind n)

          -- | Types that can't be placed in the other fields go here.
          --   INVARIANT: this list doesn't contain other TSum forms.
        , typeSumSpill          :: [Type n] }
        deriving (Show)
        

-- | Hash value used to insert types into type sums.
--   Only type constructors that can be inserted into the sum have a hash value.
data TyConHash 
        = TyConHash !Int
        deriving (Eq, Show, Ord, Ix)


-- | Wraps a variable or constructor that can be added to the `typeSumElems` array.
data TypeSumVarCon n
        = TypeSumVar (Bound n)
        | TypeSumCon (Bound n)
        deriving Show


-- TyCon ----------------------------------------------------------------------
-- | Kind, type and witness constructors.
--
--   These are grouped to make it easy to determine the universe that they
--   belong to.
-- 
data TyCon n
        -- | Sort constructors                          (level 3)
        = TyConSort     SoCon

        -- | Kind constructors                          (level 2)
        | TyConKind     KiCon

        -- | Spec constructors                          (level 1)
        | TyConSpec     TcCon

        -- | Spec constructors (for witness specs)      (level 1)
        | TyConWitness  TwCon

        -- | User defined and primitive constructors.
        | TyConBound   (Bound n)
        deriving Show


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

        -- | Purity of some effect.
        | TwConPure             -- :: ! ~> @

        -- | Emptiness of some closure.
        | TwConEmpty            -- :: $ ~> @

        -- | Globalness of some region.
        | TwConGlobal           -- :: % ~> @

        -- | Globalness of material regions in some type.
        | TwConDeepGlobal       -- :: * ~> @
        
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

        -- | Manifestness of some region (not lazy).
        | TwConManifest         -- :: % ~> @
        deriving (Eq, Show)


-- | Other constructors at the spec level.
data TcCon
        -- Data type constructors ---------------
        -- | The function type constructor is baked in so we 
        --   represent it separately.
        = TcConFun              -- '(->) :: * ~> * ~> ! ~> $ ~> *'

        -- Effect type constructors -------------
        -- | Read of some region
        | TcConRead             -- :: '% ~> !'

        -- | Read the head region in a value type.
        | TcConHeadRead         -- :: '* ~> !'

        -- | Read of all material regions in value type.
        | TcConDeepRead         -- :: '* ~> !'
        
        -- | Write of some region.
        | TcConWrite            -- :: '% ~> !'

        -- | Write to all material regions in some type.
        | TcConDeepWrite        -- :: '* ~> !'
        
        -- | Allocation into some region.
        | TcConAlloc            -- :: '% ~> !'

        -- | Allocation into all material regions in some type.
        | TcConDeepAlloc        -- :: '* ~> !'
        
        -- Closure type constructors ------------
        -- | Some region is captured in a closure.
        | TcConUse              -- :: '% ~> $'
        
        -- | All material regions in a type are captured in a closure.
        | TcConDeepUse          -- :: '* ~> $'
        deriving (Eq, Show)

