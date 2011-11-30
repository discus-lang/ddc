
module DDC.Type.Check.Env
        ( Env(..)
        , empty
        , extend
        , lookup
        , lookupName)
where
import DDC.Type.Exp
import Data.Map                 (Map)
import qualified Data.Map       as Map
import Prelude                  hiding (lookup)
import qualified Prelude        as P

-- | Type environment used when checking.
data Env n
        = Env
        { envMap        :: Map n (Type n)
        , envStack      :: [Type n] }


-- | An empty environment.
empty :: Env n
empty   = Env
        { envMap        = Map.empty
        , envStack      = [] }


-- | Extend an environment with a new binding.
extend :: Ord n => Bind n -> Env n -> Env n
extend bb env
 = case bb of
         BName n k      -> env { envMap   = Map.insert n k (envMap env) }
         BAnon   k      -> env { envStack = k : envStack env }


-- | Lookup a bound variable from an environment.
lookup :: Ord n => Bound n -> Env n -> Maybe (Type n)
lookup uu env
 = case uu of
        UName n _       -> Map.lookup n (envMap env)
        UIx i _         -> P.lookup i (zip [0..] (envStack env))


-- | Lookup a bound name from an environment.
lookupName :: Ord n => n -> Env n -> Maybe (Type n)
lookupName n env
        = Map.lookup n (envMap env)