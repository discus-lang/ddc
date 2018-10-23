
module DDC.Type.Exp.Simple.Exp
        ( Binder        (..)
        , Bind          (..)
        , Bound         (..)
        , Type          (..)
        , Sort
        , Kind
        , Region
        , Effect
        , Closure
        , TypeSum       (..)
        , TyConHash     (..)
        , TypeSumVarCon (..)
        , TyCon         (..)
        , SoCon         (..)
        , KiCon         (..)
        , TwCon         (..)
        , TcCon         (..))
where
import DDC.Type.Exp.TyCon
import DDC.Data.Label
import Data.Array
import Data.Map.Strict  (Map)
import Data.Set         (Set)


-- Binder ---------------------------------------------------------------------
data Binder n
        = RNone
        | RAnon
        | RName !n
        deriving Show


-- Bind -----------------------------------------------------------------------
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
        deriving Show


-- Types ----------------------------------------------------------------------
-- | A value type, kind, or sort.
--
--   We use the same data type to represent all three universes, as they have
--   a similar algebraic structure.
--
data Type n
        -- | Constructor.
        = TCon    !(TyCon n)

        -- | Variable.
        | TVar    !(Bound n)

        -- | Abstraction.
        | TAbs    !(Bind  n) !(Type  n)

        -- | Application.
        | TApp    !(Type  n) !(Type  n)

        -- | Universal Quantification.
        | TForall !(Bind  n) !(Type  n)

        -- | Row type, used for tuples and records.
        | TRow    ![(Label, Type n)]

        -- | Least upper bound.
        | TSum    !(TypeSum n)
        deriving Show


-- | Sorts are types at level 3.
type Sort    n = Type n

-- | Kinds are types at level 2
type Kind    n = Type n

-- | Alias for region types.
type Region  n = Type n

-- | Alias for effect types.
type Effect  n = Type n

-- | Alias for closure types.
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
        deriving Show


-- | Hash value used to insert types into the `typeSumElems` array of a `TypeSum`.
data TyConHash
        = TyConHash !Int
        deriving (Eq, Show, Ord, Ix)


-- | Wraps a variable or constructor that can be added the `typeSumElems` array.
data TypeSumVarCon n
        = TypeSumVar !(Bound n)
        | TypeSumCon !n
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

        -- | User defined type constructor.
        | TyConBound    !n

        -- | An existentially quantified name, with its kind.
        --   Used during type checking, but not accepted in source programs.
        | TyConExists   !Int       !(Kind n)
        deriving Show

