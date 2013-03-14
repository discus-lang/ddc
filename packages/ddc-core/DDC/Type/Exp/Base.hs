
module DDC.Type.Exp.Base where
import Data.Array
import Data.Map.Strict  (Map)
import Data.Set         (Set)


-- Bind -----------------------------------------------------------------------
-- | A variable binder.
data Binder n
        = RNone
        | RAnon
        | RName !n
        deriving Show


-- | A variable binder with its type.
data Bind n
        -- | A variable with no uses in the body doesn't need a name.
        = BNone     !(Type n)

        -- | Nameless variable on the deBruijn stack.
        | BAnon     !(Type n)

        -- | Named variable in the environment.
        | BName n   !(Type n)
        deriving Show



-- | A bound occurrence of a variable, with its type.
--
--   If variable hasn't been annotated with its real type then this 
--   can be `tBot` (an empty sum).

data Bound n
        -- | Nameless variable that should be on the deBruijn stack.
        = UIx   !Int   

        -- | Named variable that should be in the environment.
        | UName !n

        -- | Named primitive that has its type attached to it.
        --   The types of primitives must be closed.
        | UPrim !n !(Type n)
        deriving Show


-- Types ----------------------------------------------------------------------
-- | A value type, kind, or sort.
--
--   We use the same data type to represent all three universes, as they have
--  a similar algebraic structure.
--
data Type n
        -- | Variable.
        = TVar    !(Bound n)

        -- | Constructor.
        | TCon    !(TyCon n)

        -- | Abstraction.
        | TForall !(Bind  n) !(Type  n)
        
        -- | Application.
        | TApp    !(Type  n) !(Type  n)

        -- | Least upper bound.
        | TSum    !(TypeSum n)
        deriving Show


type Sort    n = Type n
type Kind    n = Type n
type Region  n = Type n
type Effect  n = Type n
type Closure n = Type n


-- Type Sums ------------------------------------------------------------------
-- | A least upper bound of several types.
-- 
--   We keep type sums in this normalised format instead of joining them
--   together with a binary operator (like @(+)@). This makes sums easier to work
--   with, as a given sum type often only has a single physical representation.
data TypeSum n
        = TypeSumBot
        { typeSumKind           :: !(Kind n) }

        | TypeSumSet
        { -- | The kind of the elements in this sum.
          typeSumKind           :: !(Kind n)

          -- | Where we can see the outer constructor of a type, its argument
          --   is inserted into this array. This handles common cases like
          --   Read, Write, Alloc effects.
        , typeSumElems          :: !(Array TyConHash (Set (TypeSumVarCon n)))

          -- | A map for named type variables.
        , typeSumBoundNamed     :: !(Map n   (Kind n))

          -- | A map for anonymous type variables.
        , typeSumBoundAnon      :: !(Map Int (Kind n))

          -- | Types that can't be placed in the other fields go here.
          -- 
          --   INVARIANT: this list doesn't contain more `TSum`s.
        , typeSumSpill          :: ![Type n] }
        deriving (Show)
        

-- | Hash value used to insert types into the `typeSumElems` array of a `TypeSum`.
data TyConHash 
        = TyConHash !Int
        deriving (Eq, Show, Ord, Ix)


-- | Wraps a variable or constructor that can be added the `typeSumElems` array.
data TypeSumVarCon n
        = TypeSumVar !(Bound n)
        | TypeSumCon !(Bound n) !(Type n)
        deriving Show


-- TyCon ----------------------------------------------------------------------
-- | Kind, type and witness constructors.
--
--   These are grouped to make it easy to determine the universe that they
--   belong to.
-- 
data TyCon n
        -- | (level 3) Builtin Sort constructors.
        = TyConSort     !SoCon

        -- | (level 2) Builtin Kind constructors.
        | TyConKind     !KiCon

        -- | (level 1) Builtin Spec constructors for the types of witnesses.
        | TyConWitness  !TwCon

        -- | (level 1) Builtin Spec constructors for types of other kinds.
        | TyConSpec     !TcCon

        -- | User defined and primitive constructors.
        | TyConBound   !(Bound n) !(Type n)
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
        --   This is only well formed when it is fully applied.
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
        = TwConImpl             -- :: '(=>) :: @ ~> *'

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

        -- | Distinctness of some n regions
        | TwConDistinct Int     -- :: * ~> [%] ~> @
        
        -- | Laziness of some region.
        | TwConLazy             -- :: % ~> @

        -- | Laziness of the primary region in some type.
        | TwConHeadLazy         -- :: * ~> @

        -- | Manifestness of some region (not lazy).
        | TwConManifest         -- :: % ~> @

        -- | Non-interfering effects are disjoint. Used for rewrite rules.
        | TwConDisjoint               -- :: ! ~> ! ~> @
        deriving (Eq, Show)


-- | Other constructors at the spec level.
data TcCon
        -- Data type constructors ---------------
        -- | The unit data type constructor is baked in.
        = TcConUnit             -- 'Unit :: *'

        -- | The function type constructor is baked in.
        | TcConFun              -- '(->) :: * ~> * ~> ! ~> $ ~> *'

        -- Effect type constructors -------------
        -- | Read of some region.
        | TcConRead             -- :: '% ~> !'

        -- | Read the head region in a data type.
        | TcConHeadRead         -- :: '* ~> !'

        -- | Read of all material regions in a data type.
        | TcConDeepRead         -- :: '* ~> !'
        
        -- | Write of some region.
        | TcConWrite            -- :: '% ~> !'

        -- | Write to all material regions in some data type.
        | TcConDeepWrite        -- :: '* ~> !'
        
        -- | Allocation into some region.
        | TcConAlloc            -- :: '% ~> !'

        -- | Allocation into all material regions in some data type.
        | TcConDeepAlloc        -- :: '* ~> !'
        
        -- Closure type constructors ------------
        -- | Region is captured in a closure.
        | TcConUse              -- :: '% ~> $'
        
        -- | All material regions in a data type are captured in a closure.
        | TcConDeepUse          -- :: '* ~> $'
        deriving (Eq, Show)
