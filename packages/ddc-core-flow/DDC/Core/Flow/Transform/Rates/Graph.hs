module DDC.Core.Flow.Transform.Rates.Graph
        ( Graph(..)
        , Edge(..)
        , graphOfBinds
        , graphTopoOrder 
        , mergeWeights
        , invertMap)
where
import DDC.Core.Flow.Transform.Rates.Combinators
import DDC.Core.Flow.Transform.Rates.SizeInference

import           Data.List              (nub)
import qualified Data.Map               as Map
import           Data.Maybe             (catMaybes)
import qualified Data.Set               as Set

import Control.Applicative

-- | Graph for function
--   Each node is a binding, edges are dependencies, and the bool is whether the node's output
--   can be fused or contracted.
--   For example, filter and map dependencies can be contracted,
--   but a fold cannot as it must consume the entire stream before producing output.
--

type Edge  n   = (n, Bool)
data Graph n t = Graph (Map.Map n (t, [Edge n]))


graphOfBinds :: (Ord s, Ord a) => Program s a -> Env a -> Graph (CName s a) (Maybe (Type a))
graphOfBinds prog env
 = Graph $ graph
 where
  graph
   = Map.fromList
   $ map gen
   $ _binds prog

  gen b
   = let n    = cnameOfBind b
         ty   = iter prog env n
         es   = edges n b
     in (n, (ty, es))

  edges n (ABind _ (Gather a b))
   = let a' = mkedge (const False) a
         b' = mkedge (inedge n)    b
     in catMaybes [a', b']

  edges n (ABind _ (Cross a b))
   = let a' = mkedge (inedge n)    a
         b' = mkedge (const False) b
     in catMaybes [a', b']

  edges n b
   = let fs   = freeOfBind  b
         fs'  = catMaybes
              $ map (cnameOfEither prog) fs
         fs'' = map (pairon (inedge n)) fs'
     in  fs''

  mkedge f a
   = pairon f <$> cnameOfEither prog (Right a)

  pairon f x
   = (x, f x)

  inedge to from
   -- scalar output:
   | NameScalar _ <- from
   = False
   | NameExt _    <- from
   = False
   | NameExt _    <- to
   = False
   | otherwise
   = True




-- | Find topological ordering of DAG
-- Does not check for cycles - really must be a DAG!
graphTopoOrder :: Ord n => Graph n t -> [n]
graphTopoOrder (Graph graph)
 = reverse $ go ([], Map.keysSet graph)
 where
  go (l, s)
   = case Set.minView s of
     Nothing
      -> l
     Just (m, _)
      -> go (visit (l,s) m)

  visit (l,s) m
   | Set.member m s
   , (_ty, edges) <- graph Map.! m
   = let pres     = map fst edges
         s'       = Set.delete m s
         (l',s'') = foldl visit (l,s') pres
     in (m : l', s'')

   | otherwise
   = (l,s)



-- | Merge nodes together with same value in weight map.
-- Type information of each node is thrown away.
-- It is, perhaps surprisingly, legal to merge nodes of different types (eg filtered data),
-- so the only sensible thing is to choose () for all new types.
mergeWeights :: Ord n => Graph n t -> Map.Map n Int -> Graph n ()
mergeWeights g@(Graph graph) weights
 = Graph
 $ foldl go Map.empty
 $ graphTopoOrder g
 where
  go m node
   -- Merge if it's a weighted one
   | Just k     <- name_maybe node
   = merge node k    m
   | otherwise
   = merge node node m

  merge node k m
   | Just (_ty,edges) <- Map.lookup node graph
   = let edges' = nub $ map (\(n,f) -> (name n, f)) edges
     in  Map.insertWith ins k ((),edges') m
   | otherwise
   = m

  ins (_, e1) (_, e2)
   = ((), nub $ e1 ++ e2)

  weights' = invertMap weights

  name n
   = maybe n id (name_maybe n)

  -- If this node is mentioned in the weights map, then find some canonical name for it.
  name_maybe n
   | Just i      <- Map.lookup n weights
   , Just (v:_)  <- Map.lookup i weights'
   = Just v
   | otherwise
   = Nothing


invertMap :: (Ord k, Ord v) => Map.Map k v -> Map.Map v [k]
invertMap m
 = Map.foldWithKey go Map.empty m
 where
  go k v m' = Map.insertWith (++) v [k] m'


