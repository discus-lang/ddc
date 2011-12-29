
module DDC.Type.Exp
        ( -- * Types, Kinds, and Sorts
          Type     (..)
        , Binder  (..)
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
        deriving (Eq, Show)


type Sort    n = Type n
type Kind    n = Type n
type Region  n = Type n
type Effect  n = Type n
type Closure n = Type n


-- Bind -------------------------------------------------------------------------------------------
-- | Binding occurrence of a variable.
data Bind n
        = BNone     (Type n)    -- ^ A variable with no uses in the body doesn't need a name.
        | BAnon     (Type n)    -- ^ Nameless variable on the deBruijn stack.
        | BName n   (Type n)    -- ^ Named variable in the environment.
        deriving (Eq, Show)


-- | Represents the binder of a `Bind`, without the type.
data Binder n
        = RNone
        | RAnon
        | RName n
        deriving (Eq, Show)


-- | Bound occurrence of a variable.
-- 
--   * If the variables haven't been annotated with their kinds then the kind field will be TBot. 
data Bound n
        = UIx   Int (Type n)    -- ^ Nameless variable that should be on the deBruijn stack.
        | UName n   (Type n)    -- ^ Named variable that should be in the environment.
        | UPrim n   (Type n)    -- ^ Named primitive that is not bound in the environment.
                                --   Prims aren't every counted as being free.
        deriving (Eq, Show)


instance Ord n => Ord (Bound n) where
 compare (UName n1 _) (UName n2 _)      = compare n1 n2
 compare (UIx   i1 _) (UIx   i2 _)      = compare i1 i2
 compare (UPrim n1 _) (UPrim n2 _)      = compare n1 n2
 compare (UIx   _  _) _                 = LT
 compare (UName _  _) (UIx   _ _)       = GT
 compare (UName _  _) (UPrim _ _)       = LT
 compare (UPrim _  _) _                 = GT


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
        -- | Builtin Sort constructors               (level 3)
        = TyConSort    SoCon

        -- | Builtin Kind constructors               (level 2)
        | TyConKind    KiCon

        -- | Builtin Witness type constructors       (level 1)
        | TyConWitness TwCon

        -- | Builtin Computation type constructors   (level 1)
        | TyConComp    TcCon
        
        -- | User defined and primitive constructors.
        | TyConBound   (Bound n)
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

        -- | Purity of some effect.
        | TwConPure             -- :: ! ~> @

        -- | Emptiness of some closure.
        | TwConEmpty            -- :: $ ~> @

        -- | Globalness of some region.
        | TwConGlobal           -- :: % ~> @

        -- | Globalness of material regions in some type.
        | TwConDeepGlobal       -- :: % ~> @
        
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

        -- | Distinctness \/ Separation of regions.
        --   Arity must be >= 2.
        | TwConDistinct Int     -- :: % ~> % ... ~> @

        deriving (Eq, Show)


-- | Builtin computation type constructors.
data TcCon
        -- Data type constructors ---------------
        -- | The function type constructor is baked in so we 
        --   represent it separately.
        = TcConFun              -- '(->) :: * ~> * ~> ! ~> $ ~> *'


        -- Effect type constructors -------------
        -- | Read of some region
        | TcConRead             -- :: '% ~> !'

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

