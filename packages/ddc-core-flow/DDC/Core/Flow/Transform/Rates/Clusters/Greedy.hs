module DDC.Core.Flow.Transform.Rates.Clusters.Greedy
    (cluster_greedy)
 where

import DDC.Base.Pretty
import DDC.Core.Flow.Transform.Rates.Graph
import DDC.Core.Flow.Transform.Rates.Clusters.Base

cluster_greedy :: (Ord n, Eq t, Show n, Pretty n) => Graph n t -> TransducerMap n -> [[n]]
cluster_greedy g trans
 -- First find a greedy vertical clustering, then merge any leftover horizontal opportunities
 --
 -- The clusters are built in reverse order, so fix them up.
 = reverse
 $ map reverse
 $ fuse_rest vertical
 where
  -- Vertical fusion.
  -- Go through the graph in topo order, inserting each node into a cluster
  vertical
   = foldl go_vertical []
   $ graphTopoOrder g

  -- Insert node n into the clusters ns
  go_vertical ns n
         -- Find the parent we'd like to fuse n into. It has to be a fusible edge.
   = let parent = [ n' | (n',fusible) <- nodeInEdges g n, fusible]
         -- The default unfused clustering to use if we can't merge n into parent
         unfused  = [n] : ns
     in  case parent of
          -- There is no parent (that is fusible)
          []
           -> unfused
          -- Check that n and n' are the same type (can be fused).
          (n':_)
           | tcmp n n'
           -> case insert n n' ns of
               Just ns' -> ns'
               Nothing  -> unfused
           | otherwise
           -> unfused

  -- Try to insert n into the same cluster as n'.
  -- If n relies on output of clusters after n' (before in cs - list is reversed), we cannot put
  -- n in the n' cluster.
  -- Also check that there are no fusion-preventing paths between cluster and n.

  -- We haven't seen n' yet and we're at the end, so can't put n into same cluster as n'
  insert _n _n' []
   = Nothing

  insert n n' (c:cs) 
   -- We've reached n' cluster.
   -- If there are no fusion-preventing edges between n and these nodes, we can fuse.
   | n' `elem` c
   = if   any (not . checkPath n) c
     then Nothing
     else Just ((n:c) : cs)

   -- If n relies on any of these nodes and we haven't reached n',
   -- we won't be able to merge n with n'
   -- (because it would not be able to execute without result of these nodes)
   | any (edge n) c
   = Nothing

   | otherwise
   = do cs' <- insert n n' cs
        return (c : cs')


  -- Do any leftover horizontal fusion that we can
  fuse_rest [] = []
  fuse_rest (c:cs)
   = case try_merge c cs of
      Nothing
       -> c : fuse_rest cs
      Just cs'
       -> fuse_rest cs'

  -- Nothing to merge s with
  try_merge _ []
   = Nothing

  try_merge s (c:cs)
   -- Can s and c be merged together?
   | miscible s c
   = if   all (\a -> all (checkPath a) s) c
     then Just ((s ++ c) : cs)
     else Nothing

   -- s and c can't be merged together, but we can't move s any further back
   | any (\a -> any (edge a) s) c
   = Nothing

   | otherwise
   = do cs' <- try_merge s cs
        return (c : cs')


  -- Helper functions
  edge a b = hasEdge g (a,b) || hasEdge g (b,a)

  checkPath = noFusionPreventingPath arcs
  arcs = snd $ listOfGraph g

  tcmp = typeComparable g trans

  -- Check if two clusters can be merged together
  miscible s c
   = let sc = s ++ c
         -- For each a in c and b in s
     in  all (\a -> all (\b ->
           -- If a and b have same type, they can be merged fine
           case (nodeType g a, nodeType g b) of
                (Just ta, Just tb)
                 -> if   ta == tb
                    then True
                    -- If they have different types, but share parent transducers,
                    -- they can only be merged if they are merged with both parents.
                    else case trans a b of
                     Just (a',b')  -> a' `elem` sc && b' `elem` sc
                     Nothing       -> False
                _
                  -> False
            ) s) c

