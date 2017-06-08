module DDC.Core.Flow.Transform.Rates.Clusters.Base where
import DDC.Core.Flow.Transform.Rates.Graph

type TransducerMap n = n -> n -> Maybe (n,n)

-- \forall paths p from u to v, fusion preventing \not\in p
noFusionPreventingPath :: (Ord n) => [((n,n),Bool)] -> n -> n -> Bool
noFusionPreventingPath arcs u v
 -- for all paths, for all nodes in path, is fusible
 =  all (all snd) (paths u v)
 && all (all snd) (paths v u)
 where
  -- list of all paths from w to x
  paths w x
    | w == x
    = [[]]
    | otherwise
    = let outs = filter (\((i,_j),_f) -> i == w) arcs
      in  concatMap (\((w',j),f) -> map (((w',j),f):) (paths j x)) outs
   

-- | Check if two nodes may be fused based on type.
-- If they have the same type, it's fine.
-- If they have a different type, we must look for any common type transducer parents.
typeComparable :: (Ord n, Eq t) => Graph n t -> TransducerMap n -> n -> n -> Bool
typeComparable g trans a b
 = case (nodeType g a, nodeType g b) of
   (Just a', Just b')
    -> if   a' == b'
       then True
       else case trans a b of
                 Just _  -> True
                 Nothing -> False
   _
    -> False


