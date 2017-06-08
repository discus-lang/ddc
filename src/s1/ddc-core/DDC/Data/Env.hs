
-- | Generic environment that handles both named and anonymous
--   de-bruijn binders.
module DDC.Data.Env
        ( -- * Types
          Bind   (..)
        , Bound  (..)
        , Env    (..)

          -- * Conversion
        , fromList
        , fromNameList
        , fromNameMap

          -- * Constructors
        , empty
        , singleton
        , extend,       extends
        , union,        unions

          -- * Lookup
        , member
        , lookup
        , lookupName,   lookupIx
        , depth)
where
import Data.Maybe
import Data.Map                         (Map)
import qualified Data.Map.Strict        as Map
import qualified Prelude                as P
import Prelude                          hiding (lookup)


-------------------------------------------------------------------------------
-- | A binding occurrence of a variable.
data Bind n
        -- | No binding, or alternatively, bind a fresh name that has
        --   no bound uses.
        = BNone

        -- | Anonymous binder.
        | BAnon

        -- | Named binder.
        | BName !n
        deriving (Eq, Ord, Show)


-- | A bound occurrence of a variable.
data Bound n
        -- | Index an anonymous binder.
        = UIx   !Int

        -- | Named variable.
        | UName !n
        deriving (Eq, Ord, Show)


-- | Generic environment that maps a variable to a thing of type @a@. 
data Env n a
        = Env
        { -- | Named things.
          envMap         :: !(Map n a)

          -- | Anonymous things.
        , envStack       :: ![a] 
        
          -- | Length of the stack.
        , envStackLength :: !Int }


-------------------------------------------------------------------------------
-- | Convert a list of `Bind`s to an environment.
fromList :: Ord n => [(Bind n, a)] -> Env n a
fromList bs
        = foldr (uncurry extend) empty bs


-- | Convert a list of `Bind`s to an environment.
fromNameList :: Ord n => [(n, a)] -> Env n a
fromNameList bs
        = foldr (uncurry extend) empty 
        $ [(BName n, x) | (n, x) <- bs ]


-- | Convert a map of things to an environment.
fromNameMap :: Map n a -> Env n a
fromNameMap m
        = empty { envMap = m }


---------------------------------------------------------------------------------
-- | An empty environment, with nothing in it.
empty :: Env n a
empty   = Env
        { envMap         = Map.empty
        , envStack       = [] 
        , envStackLength = 0 }


-- | Construct a singleton environment.
singleton :: Ord n => Bind n -> a -> Env n a
singleton b x
        = extend b x empty


-- | Extend an environment with a new binding.
--   Replaces bindings with the same name already in the environment.
extend :: Ord n => Bind n -> a -> Env n a -> Env n a
extend bb x env
 = case bb of
         BNone{}        -> env

         BAnon          -> env { envStack       = x : envStack env 
                               , envStackLength = envStackLength env + 1 }

         BName n        -> env { envMap         = Map.insert n x (envMap env) }


-- | Extend an environment with a list of new bindings.
--   Replaces bindings with the same name already in the environment.
extends :: Ord n => [(Bind n, a)] -> Env n a -> Env n a
extends bs env
        = foldl (flip (uncurry extend)) env bs


-- | Combine two environments.
--   If both environments have a binding with the same name,
--   then the one in the second environment takes preference.
union :: Ord n => Env n a -> Env n a -> Env n a
union env1 env2
        = Env  
        { envMap         = envMap env1 `Map.union` envMap env2
        , envStack       = envStack       env2  ++ envStack       env1
        , envStackLength = envStackLength env2  +  envStackLength env1 }


-- | Combine multiple environments,
--   with the latter ones taking preference.
unions :: Ord n => [Env n a] -> Env n a
unions envs
        = foldr union empty envs


-- | Check whether a bound variable is present in an environment.
member :: Ord n => Bound n -> Env n a -> Bool
member uu env
        = isJust $ lookup uu env


-- | Lookup a bound variable from an environment.
lookup :: Ord n => Bound n -> Env n a -> Maybe a
lookup uu env
 = case uu of
        UIx i           -> P.lookup i (zip [0..] (envStack env))
        UName n         -> Map.lookup n (envMap env) 


-- | Lookup a value from the environment based on its name.
lookupName :: Ord n => n -> Env n a -> Maybe a
lookupName n env
        = Map.lookup n (envMap env)


-- | Lookup a value from the environment based on its index.
lookupIx :: Ord n => Int -> Env n a -> Maybe a
lookupIx ix env
        = P.lookup ix (zip [0..] (envStack env))


-- | Yield the total depth of the anonymous stack.
depth :: Env n a -> Int
depth env       = envStackLength env

