
-- | Type environments.
--
--   An environment contains the types 
--     named bound variables,
--     named primitives, 
--     and a deBruijn stack for anonymous variables.
--
module DDC.Type.Env
        ( Env(..)
        , empty
        , extend,       extends
        , setPrimFun,   isPrim
        , fromList
        , union
        , member,       memberBind
        , lookup,       lookupName
        , depth
        , lift
        , wrapTForalls)
where
import DDC.Type.Exp
import DDC.Type.Transform.LiftT
import Data.Maybe
import Data.Map                 (Map)
import Prelude                  hiding (lookup)
import qualified Data.Map       as Map
import qualified Prelude        as P
import Control.Monad


-- | A type environment.
data Env n
        = Env
        { -- | Types of named binders.
          envMap         :: Map n (Type n)

          -- | Types of anonymous deBruijn binders.
        , envStack       :: [Type n] 
        
          -- | The length of the above stack.
        , envStackLength :: Int

          -- | Types of baked in, primitive names.
        , envPrimFun     :: n -> Maybe (Type n) }


-- | An empty environment.
empty :: Env n
empty   = Env
        { envMap         = Map.empty
        , envStack       = [] 
        , envStackLength = 0
        , envPrimFun     = \_ -> Nothing }


-- | Extend an environment with a new binding.
--   Replaces bindings with the same name already in the environment.
extend :: Ord n => Bind n -> Env n -> Env n
extend bb env
 = case bb of
         BName n k      -> env { envMap         = Map.insert n k (envMap env) }
         BAnon   k      -> env { envStack       = k : envStack env 
                               , envStackLength = envStackLength env + 1 }
         BNone{}        -> env


-- | Extend an environment with a list of new bindings.
--   Replaces bindings with the same name already in the environment.
extends :: Ord n => [Bind n] -> Env n -> Env n
extends bs env
        = foldl (flip extend) env bs


-- | Set the function that knows the types of primitive things.
setPrimFun :: (n -> Maybe (Type n)) -> Env n -> Env n
setPrimFun f env
        = env { envPrimFun = f }


-- | Check if the type of a name is defined by the `envPrimFun`.
isPrim :: Env n -> n -> Bool
isPrim env n
        = isJust $ envPrimFun env n


-- | Convert a list of `Bind`s to an environment.
fromList :: Ord n => [Bind n] -> Env n
fromList bs
        = foldr extend empty bs


-- | Combine two environments.
--   If both environments have a binding with the same name,
--   then the one in the second environment takes preference.
union :: Ord n => Env n -> Env n -> Env n
union env1 env2
        = Env  
        { envMap         = envMap env1 `Map.union` envMap env2
        , envStack       = envStack       env2  ++ envStack       env1
        , envStackLength = envStackLength env2  +  envStackLength env1
        , envPrimFun     = \n -> envPrimFun env2 n `mplus` envPrimFun env1 n }


-- | Check whether a bound variable is present in an environment.
member :: Ord n => Bound n -> Env n -> Bool
member uu env
        = isJust $ lookup uu env


-- | Check whether a binder is already present in the an environment.
--   This can only return True for named binders, not anonymous or primitive ones.
memberBind :: Ord n => Bind n -> Env n -> Bool
memberBind uu env
 = case uu of
        BName n _       -> Map.member n (envMap env)
        _               -> False


-- | Lookup a bound variable from an environment.
lookup :: Ord n => Bound n -> Env n -> Maybe (Type n)
lookup uu env
 = case uu of
        UName n _
         ->      Map.lookup n (envMap env) 
         `mplus` envPrimFun env n

        UIx i _ 
         -> P.lookup i (zip [0..] (envStack env))

        UPrim n _
         -> envPrimFun env n


-- | Lookup a bound name from an environment.
lookupName :: Ord n => n -> Env n -> Maybe (Type n)
lookupName n env
        = Map.lookup n (envMap env)


-- | Yield the total depth of the deBruijn stack.
depth :: Env n -> Int
depth env       = envStackLength env


-- | Lift all free deBruijn indices in the environment by the given number of steps.
--   TODO: Delay this, only lift when we extract the final type.
--         will also need to update the 'member' function.
lift  :: Ord n => Int -> Env n -> Env n
lift n env
        = Env
        { envMap         = Map.map (liftT n) (envMap env)
        , envStack       = map (liftT n) (envStack env)
        , envStackLength = envStackLength env
        , envPrimFun     = envPrimFun     env }


-- | Wrap locally bound (non primitive) variables defined in an environment
--   around a type as new foralls.
wrapTForalls :: Ord n => Env n -> Type n -> Type n
wrapTForalls env tBody
 = let  bsNamed = [BName b t | (b, t) <- Map.toList $ envMap env ]
        bsAnon  = [BAnon t   | t <- envStack env]
        
        tInner  = foldr TForall tBody (reverse bsAnon)
   in   foldr TForall tInner bsNamed

