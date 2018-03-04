
-- | Environment of a type expression.
--
--   An environment contains the types
--     named bound variables,
--     named primitives,
--     and a deBruijn stack for anonymous variables.
--
module DDC.Core.Env.EnvT
        ( EnvT(..)

        -- * Construction
        , empty
        , singleton
        , extend,       extends
        , union,        unions

        -- * Conversion
        , fromList
        , fromListNT
        , fromTypeMap

        , kindEnvOfEnvT

        -- * Projections
        , depth
        , member,       memberBind
        , lookup,       lookupName

        -- * Primitives
        , setPrimFun
        , isPrim

        -- * Lifting
        , lift)
where
import DDC.Type.Exp
import DDC.Type.Transform.BoundT
import Data.Maybe
import Data.Map                         (Map)
import Prelude                          hiding (lookup)
import qualified DDC.Type.Env           as Env
import qualified Data.Map.Strict        as Map
import qualified Prelude                as P
import Control.Monad


-- | A type environment.
data EnvT n
        = EnvT
        { -- | Types of baked in, primitive names.
          envtPrimFun      :: !(n -> Maybe (Type n))

          -- | Map of constructor name to bound type for type equations.
        , envtEquations    :: !(Map n (Type n))

          -- | Map of globally available capabilities.
        , envtCapabilities :: !(Map n (Type n))

          -- | Kinds of named variables and constructors.
        , envtMap          :: !(Map n (Type n))

          -- | Types of anonymous deBruijn variables.
        , envtStack        :: ![Type n]

          -- | The length of the above stack.
        , envtStackLength  :: !Int }


-- | An empty environment.
empty :: EnvT n
empty   = EnvT
        { envtPrimFun      = \_ -> Nothing
        , envtEquations    = Map.empty
        , envtCapabilities = Map.empty
        , envtMap          = Map.empty
        , envtStack        = []
        , envtStackLength  = 0 }


-- | Construct a singleton type environment.
singleton :: Ord n => Bind n -> EnvT n
singleton b
        = extend b empty


-- | Extend an environment with a new binding.
--   Replaces bindings with the same name already in the environment.
extend :: Ord n => Bind n -> EnvT n -> EnvT n
extend bb env
 = case bb of
         BName n k -> env { envtMap         = Map.insert n k (envtMap env) }
         BAnon   k -> env { envtStack       = k : envtStack env
                          , envtStackLength = envtStackLength env + 1 }
         BNone{}   -> env


-- | Extend an environment with a list of new bindings.
--   Replaces bindings with the same name already in the environment.
extends :: Ord n => [Bind n] -> EnvT n -> EnvT n
extends bs env
        = foldl (flip extend) env bs


-- | Set the function that knows the types of primitive things.
setPrimFun :: (n -> Maybe (Type n)) -> EnvT n -> EnvT n
setPrimFun f env
        = env { envtPrimFun = f }


-- | Check if the type of a name is defined by the `envPrimFun`.
isPrim :: EnvT n -> n -> Bool
isPrim env n
        = isJust $ envtPrimFun env n


-- | Convert a list of `Bind`s to an environment.
fromList :: Ord n => [Bind n] -> EnvT n
fromList bs
        = foldr extend empty bs


-- | Convert a list of name and types into an environment
fromListNT :: Ord n => [(n, Type n)] -> EnvT n
fromListNT nts
 = fromList [BName n t | (n, t) <- nts]


-- | Convert a map of names to types to a environment.
fromTypeMap :: Map n (Type n) -> EnvT n
fromTypeMap m
        = empty { envtMap = m}


-- | Extract a `KindEnv` from an `EnvT`.
kindEnvOfEnvT :: Ord n => EnvT n -> Env.KindEnv n
kindEnvOfEnvT env
        = Env.empty
        { Env.envMap       = envtMap env
        , Env.envPrimFun   = \n -> envtPrimFun env n }


-- | Combine two environments.
--   If both environments have a binding with the same name,
--   then the one in the second environment takes preference.
union :: Ord n => EnvT n -> EnvT n -> EnvT n
union env1 env2
        = EnvT
        { envtMap          = envtMap          env1 `Map.union` envtMap          env2
        , envtStack        = envtStack        env2  ++ envtStack                env1
        , envtStackLength  = envtStackLength  env2  +  envtStackLength          env1
        , envtEquations    = envtEquations    env1 `Map.union` envtEquations    env2
        , envtCapabilities = envtCapabilities env1 `Map.union` envtCapabilities env2
        , envtPrimFun      = \n -> envtPrimFun env2 n `mplus` envtPrimFun env1 n }


-- | Combine multiple environments,
--   with the latter ones taking preference.
unions :: Ord n => [EnvT n] -> EnvT n
unions envs
        = foldr union empty envs


-- | Check whether a bound variable is present in an environment.
member :: Ord n => Bound n -> EnvT n -> Bool
member uu env
        = isJust $ lookup uu env


-- | Check whether a binder is already present in the an environment.
--   This can only return True for named binders, not anonymous or primitive ones.
memberBind :: Ord n => Bind n -> EnvT n -> Bool
memberBind uu env
 = case uu of
        BName n _ -> Map.member n (envtMap env)
        _         -> False


-- | Lookup a bound variable from an environment.
lookup :: Ord n => Bound n -> EnvT n -> Maybe (Type n)
lookup uu env
 = case uu of
        UName n
         ->      Map.lookup n (envtMap env)
         `mplus` envtPrimFun env n

        UIx i    -> P.lookup i (zip [0..] (envtStack env))
        UPrim n  -> envtPrimFun env n


-- | Lookup a bound name from an environment.
lookupName :: Ord n => n -> EnvT n -> Maybe (Type n)
lookupName n env
          = Map.lookup n (envtMap env)


-- | Yield the total depth of the deBruijn stack.
depth :: EnvT n -> Int
depth env = envtStackLength env


-- | Lift all free deBruijn indices in the environment by the given number of steps.
---
--  ISSUE #276: Delay lifting of indices in type environments.
--      The 'lift' function on type environments applies to every member of
--      the environment. We'd get better complexity by recording how many
--      levels all types should be lifted by, and only applying the real lift
--      function when the type is finally extracted.
--
lift :: Ord n => Int -> EnvT n -> EnvT n
lift n env
        = EnvT
        { envtMap          = Map.map (liftT n) (envtMap env)
        , envtStack        = map (liftT n) (envtStack env)
        , envtStackLength  = envtStackLength  env
        , envtEquations    = envtEquations    env
        , envtCapabilities = envtCapabilities env
        , envtPrimFun      = envtPrimFun      env }

