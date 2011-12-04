
module DDCI.Core.Prim.Region
        (makeDefaultRegionEnv)
where
import DDCI.Core.Prim.Name
import DDC.Type.Exp
import DDC.Type.Env                     (Env)
import qualified DDC.Type.Env           as Env
import qualified DDC.Type.Compounds     as T
import qualified Data.Set               as Set
import Data.Set                         (Set)
import Data.List

import Data.Maybe


-- | Given the set of names free in some expression, 
--   construct an environment that binds all the names ending in '\'' 
--   as new regions.
makeDefaultRegionEnv :: Set (Bound Name) -> Env Name
makeDefaultRegionEnv fus
 = let  us'     = map (T.replaceTypeOfBound T.kRegion)
                $ Set.toList
                $ Set.filter 
                       (\b -> case T.takeNameOfBound b of
                                Just (Name n)   -> isSuffixOf "'" n
                                _               -> False)
                $ fus

   in   Env.fromList
                $ mapMaybe
                       (\u -> case u of
                                UName n t       -> Just $ BName n t
                                _               -> Nothing)
                $ us'
