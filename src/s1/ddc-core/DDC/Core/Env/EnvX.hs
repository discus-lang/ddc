
-- | Environment of a type expression.
--
--   An environment contains the types
--     named bound variables,
--     named primitives,
--     and a deBruijn stack for anonymous variables.
--
module DDC.Core.Env.EnvX
        ( EnvX(..), EnvT.EnvT(..)

        -- * Construction
        , empty,        fromPrimEnvs
        , singleton
        , extendX,      extendsX
        , extendT,      extendsT
        , union,        unions

        -- * Conversion
        , fromList
        , fromListNT
        , fromTypeMap

        , kindEnvOfEnvX
        , typeEnvOfEnvX

        -- * Projections
        , depth
        , lookupT
        , lookupX,      lookupNameX
        , memberX,      memberBindX

        -- * Primitives
        , setPrimFun
        , isPrim

        -- * Lifting
        , lift)
where
import DDC.Type.Exp
import DDC.Type.Transform.BoundT
import Data.Maybe
import DDC.Type.DataDef                 (DataDefs)
import Data.Map                         (Map)
import Prelude                          hiding (lookup)
import DDC.Core.Env.EnvT                (EnvT)
import qualified DDC.Type.Env           as Env
import qualified DDC.Type.DataDef       as DataDef
import qualified DDC.Core.Env.EnvT      as EnvT
import qualified Data.Map.Strict        as Map
import qualified Prelude                as P
import Control.Monad


-- | Environment of term expressions.
data EnvX n
        = EnvX
        { -- | Environment of type expressions.
          envxEnvT         :: EnvT n

          -- | Types of baked in, primitive names.
        , envxPrimFun      :: !(n -> Maybe (Type n))

          -- | Data type definitions.
        , envxDataDefs     :: !(DataDefs n)

          -- | Types of named variables and constructors.
        , envxMap          :: !(Map n (Type n))

          -- | Types of anonymous deBruijn variables.
        , envxStack        :: ![Type n]

          -- | The length of the above stack.
        , envxStackLength  :: !Int }


-- | An empty environment.
empty :: EnvX n
empty   = EnvX
        { envxEnvT         = EnvT.empty
        , envxPrimFun      = \_ -> Nothing
        , envxDataDefs     = DataDef.emptyDataDefs
        , envxMap          = Map.empty
        , envxStack        = []
        , envxStackLength  = 0 }


-- | Build an `EnvX` from prim environments.
fromPrimEnvs
        :: Ord n
        => Env.KindEnv n        -- ^ Primitive kind environment.
        -> Env.TypeEnv n        -- ^ Primitive type environment.
        -> DataDefs n           -- ^ Primitive data type definitions.
        -> EnvX n

fromPrimEnvs kenv tenv defs
 = let  envt    = EnvT.empty
                { EnvT.envtPrimFun = Env.envPrimFun kenv }

        envx    = empty
                { envxEnvT         = envt
                , envxPrimFun      = Env.envPrimFun tenv
                , envxDataDefs     = defs }
   in   envx


-- | Construct a singleton type environment.
singleton :: Ord n => Bind n -> EnvX n
singleton b
        = extendX b empty


-------------------------------------------------------------------------------
-- | Extend an environment with a new binding.
--   Replaces bindings with the same name already in the environment.
extendX :: Ord n => Bind n -> EnvX n -> EnvX n
extendX bb env
 = case bb of
         BName n k -> env { envxMap         = Map.insert n k (envxMap env) }
         BAnon   k -> env { envxStack       = k : envxStack env
                          , envxStackLength = envxStackLength env + 1 }
         BNone{}   -> env


-- | Extend an environment with a list of new bindings.
--   Replaces bindings with the same name already in the environment.
extendsX :: Ord n => [Bind n] -> EnvX n -> EnvX n
extendsX bs env
        = foldl (flip extendX) env bs


-- | Extend the environment with the kind of a new type variable.
extendT :: Ord n => Bind n -> EnvX n -> EnvX n
extendT bb envx
 = envx { envxEnvT = EnvT.extend bb (envxEnvT envx) }


-- | Extend the environment with some new type bindings.
extendsT :: Ord n => [Bind n] -> EnvX n -> EnvX n
extendsT bs env
        = foldl (flip extendT) env bs


-------------------------------------------------------------------------------
-- | Set the function that knows the types of primitive things.
setPrimFun :: (n -> Maybe (Type n)) -> EnvX n -> EnvX n
setPrimFun f env
        = env { envxPrimFun = f }


-- | Check if the type of a name is defined by the `envPrimFun`.
isPrim :: EnvX n -> n -> Bool
isPrim env n
        = isJust $ envxPrimFun env n


-- | Convert a list of `Bind`s to an environment.
fromList :: Ord n => [Bind n] -> EnvX n
fromList bs
        = foldr extendX empty bs


-- | Convert a list of name and types into an environment
fromListNT :: Ord n => [(n, Type n)] -> EnvX n
fromListNT nts
        = fromList [BName n t | (n, t) <- nts]


-- | Convert a map of names to types to a environment.
fromTypeMap :: Map n (Type n) -> EnvX n
fromTypeMap m
        = empty { envxMap = m }


-- | Extract a `KindEnv` from an EnvX.
kindEnvOfEnvX :: Ord n => EnvX n -> Env.KindEnv n
kindEnvOfEnvX env
        = EnvT.kindEnvOfEnvT $ envxEnvT env


-- | Extract `TypeEnv` from an `EnvX.
typeEnvOfEnvX :: Ord n => EnvX n -> Env.TypeEnv n
typeEnvOfEnvX env
        = Env.empty
        { Env.envMap       = envxMap env
        , Env.envPrimFun   = \n -> envxPrimFun env n }


-- | Combine two environments.
--   If both environments have a binding with the same name,
--   then the one in the second environment takes preference.
union :: Ord n => EnvX n -> EnvX n -> EnvX n
union env1 env2
        = EnvX
        { envxEnvT         = envxEnvT          env1 `EnvT.union` envxEnvT        env2
        , envxPrimFun      = \n -> envxPrimFun env2 n `mplus`   envxPrimFun env1 n
        , envxDataDefs     = envxDataDefs      env1 `mappend`   envxDataDefs     env2
        , envxMap          = envxMap           env1 `Map.union` envxMap          env2
        , envxStack        = envxStack         env2  ++ envxStack                env1
        , envxStackLength  = envxStackLength   env2  +  envxStackLength          env1 }


-- | Combine multiple environments,
--   with the latter ones taking preference.
unions :: Ord n => [EnvX n] -> EnvX n
unions envs
        = foldr union empty envs


-- | Check whether a bound variable is present in an environment.
memberX :: Ord n => Bound n -> EnvX n -> Bool
memberX uu env
        = isJust $ lookupX uu env


-- | Check whether a binder is already present in the an environment.
--   This can only return True for named binders, not anonymous or primitive ones.
memberBindX :: Ord n => Bind n -> EnvX n -> Bool
memberBindX uu env
 = case uu of
        BName n _ -> Map.member n (envxMap env)
        _         -> False


-- | Lookup a bound variable from an environment.
lookupT :: Ord n => Bound n -> EnvX n -> Maybe (Kind n)
lookupT uu env
 = EnvT.lookup uu (envxEnvT env)


-- | Lookup a bound variable from an environment.
lookupX :: Ord n => Bound n -> EnvX n -> Maybe (Type n)
lookupX uu env
 = case uu of
        UName n
         ->      Map.lookup n (envxMap env)
         `mplus` envxPrimFun env n

        UIx i   -> P.lookup i (zip [0..] (envxStack env))
        UPrim n -> envxPrimFun env n


-- | Lookup a bound name from an environment.
lookupNameX :: Ord n => n -> EnvX n -> Maybe (Type n)
lookupNameX n env
          = Map.lookup n (envxMap env)


-- | Yield the total depth of the deBruijn stack.
depth :: EnvX n -> Int
depth env = envxStackLength env


-- | Lift all free deBruijn indices in the environment by the given number of steps.
---
--  ISSUE #276: Delay lifting of indices in type environments.
--      The 'lift' function on type environments applies to every member of
--      the environment. We'd get better complexity by recording how many
--      levels all types should be lifted by, and only applying the real lift
--      function when the type is finally extracted.
--
lift :: Ord n => Int -> EnvX n -> EnvX n
lift n env
        = EnvX
        { envxEnvT         = envxEnvT env
        , envxPrimFun      = envxPrimFun      env
        , envxDataDefs     = envxDataDefs     env
        , envxMap          = Map.map (liftT n) (envxMap env)
        , envxStack        = map (liftT n) (envxStack env)
        , envxStackLength  = envxStackLength  env }

