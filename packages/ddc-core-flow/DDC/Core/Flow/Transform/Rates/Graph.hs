module DDC.Core.Flow.Transform.Rates.Graph
        ( Graph
        , Edge
        , graphOfBinds
        , graphTopoOrder 
        , mergeWeights
        , traversal
        , invertMap
        , mlookup )
where
import DDC.Core.Collect.Free
import DDC.Core.Collect.Free.Simple ()
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Exp
import qualified DDC.Type.Env           as Env

import           Data.List              (intersect, nub)
import qualified Data.Map               as Map
import           Data.Maybe             (catMaybes)
import qualified Data.Set               as Set

-- | Graph for function
-- Each node is a binding, edges are dependencies, and the bool is whether the node's output can be fused or contracted.
-- For example, filter and map dependencies can be contracted,
-- but a fold cannot as it must consume the entire stream before producing output.
--

type Edge  = (Name, Bool)
type Graph = Map.Map Name [Edge]

graphOfBinds :: [(Name,ExpF)] -> [Name] -> Graph
graphOfBinds binds extra_names
 = Map.map mkEdges graph1
 where
  mkEdges (refs, _fusible)
   = map getFusible refs
  
  getFusible r
   | Just (_,f) <- Map.lookup r graph1
   = (r, f)
   | otherwise
   = (r, True)

  graph1
   = Map.fromList
   $ map gen
   $ binds

  gen (k, xx)
   = let free = catMaybes
              $ map takeNameOfBound
              $ Set.toList
              $ freeX Env.empty xx
         refs = free `intersect` names
     in  (k, (refs, fusible xx))

  names = map fst binds ++ extra_names

  fusible xx
   | Just (f, _)                      <- takeXApps xx
   , XVar (UPrim (NameOpVector ov) _) <- f
   = case ov of
     OpVectorReduce
      -> False
     -- TODO length of `concrete rate' is known before iteration, so should be contractible.
     OpVectorLength
      -> False
     _
      -> True

   | otherwise
   = True


-- | Find topological ordering of DAG
-- Does not check for cycles - really must be a DAG!
graphTopoOrder :: Graph -> [Name]
graphTopoOrder graph
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
   = let edges    = mlookup "visit" graph m
         pres     = map fst edges
         s'       = Set.delete m s
         (l',s'') = foldl visit (l,s') pres
     in (m : l', s'')

   | otherwise
   = (l,s)



traversal :: Graph -> (Edge -> Name -> Int) -> Map.Map Name Int
traversal graph weight
 = foldl go Map.empty
 $ graphTopoOrder graph
 where
  go m node
   = let pres  = mlookup "traversal" graph node

         get e@(u,_)
          | Just v <- Map.lookup u m
          = v + weight e node
          | otherwise
          = 0

         w     = foldl max 0
               $ map get
               $ pres

     in  Map.insert node w m


mergeWeights :: Graph -> Map.Map Name Int -> Graph
mergeWeights graph weights
 = foldl go Map.empty
 $ graphTopoOrder graph
 where
  go m node
   | insertp node
   , Just edges <- Map.lookup node graph
   = Map.insert node (nub $ map (\(n,f) -> (name n, f)) edges) m
   | otherwise
   = m

  weights' = invertMap weights

  name n
   | Just i      <- Map.lookup n weights
   , Just (v:_)  <- Map.lookup i weights'
   = v
   | otherwise
   = n

  insertp n
   | Just i      <- Map.lookup n weights
   , Just vs     <- Map.lookup i weights'
   = n `elem` vs
   | otherwise
   = False

invertMap :: (Ord k, Ord v) => Map.Map k v -> Map.Map v [k]
invertMap m
 = Map.foldWithKey go Map.empty m
 where
  go k v m' = Map.insertWith (++) v [k] m'


mlookup :: Ord k => String -> Map.Map k v -> k -> v
mlookup str m k
 | Just v <- Map.lookup k m
 = v
 | otherwise
 = error ("mlookup: no key " ++ str)


