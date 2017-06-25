{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP       #-}
module DDC.Core.Flow.Transform.Rates.Clusters.Linear
    (solve_linear)
 where
#if DDC_FLOW_HAVE_LINEAR_SOLVER

import DDC.Data.Pretty
import DDC.Core.Flow.Transform.Rates.Graph
import DDC.Core.Flow.Transform.Rates.Clusters.Base

import qualified Data.Map  as Map

import Numeric.Limp.Program.ResultKind
import Numeric.Limp.Program
import Numeric.Limp.Rep
import Numeric.Limp.Solvers.Cbc.Solve

import qualified Numeric.Limp.Canon.Convert as Conv
import qualified Numeric.Limp.Canon.Simplify as CSimp


-- | Get parent transducers of two nodes, if exists.
-- | Integer-valued variable type for linear program
--
-- SameCluster i j  - {0,1} 0 if i and j are fused together
--
-- C n - 0 if node is fused with all its users. Minimising this is good for array contraction.
--
data ZVar n
 = SameCluster n n
 | C n
 deriving (Eq, Show, Ord)

instance Pretty n => Pretty (ZVar n) where
 ppr (SameCluster a b) = text "SC" <+> ppr a <+> ppr b
 ppr (C a)             = text "C"  <+> ppr a


-- | Variable type for linear program
-- Pi i             - {0..} used to show the resulting partition is acyclic:
--
--      A graph is acyclic iff there is a mapping
--          pi : Node -> Int
--      such that \forall (i,j) \in Edge
--          pi(j) > pi(i)
--
data RVar n
 = Pi n
 deriving (Eq, Show, Ord)

instance Pretty n => Pretty (RVar n) where
 ppr (Pi a) = text "O" <+> ppr a

-- | Canonical form of SameCluster variables.
-- Since we only want to generate one SameCluster variable for each pair, we generate the
-- variable with min variable then max.
mkSameCluster :: Ord n => n -> n -> ZVar n
mkSameCluster m n
 = SameCluster (min m n) (max m n)

-- | Minimise objective:
-- \Sigma_i,j Weight(i,j) * SameCluster(i,j)
gobjective :: Ord n => [n] -> [(Int,n,n)] -> Linear (ZVar n) (RVar n) IntDouble 'KZ
gobjective ns ws
 =  foldl (.+.) c0
 (  map (\(w,i,j) -> z (mkSameCluster i j) (Z w)) ws
 ++ map (\n -> z (C n) (Z $ length ns)) ns)


-- | Get variable bounds - Pi are unbounded, SameCluster and C are "bools" (0 or 1)
getBounds :: Ord n => [n] -> [(Int,n,n)] -> [Bounds (ZVar n) (RVar n) IntDouble]
getBounds ns ws
 =  map boundC  ns
 ++ map boundSC ws
 where
  boundC n
   = binary (C n)
  boundSC (_,i,j)
   = binary (mkSameCluster i j)


-- | Create constraints for edges and weights
getConstraints
        :: (Ord n, Eq t)
        => Int -> Graph n t
        -> [((n,n),Bool)]
        -> [(Int,n,n)]
        -> TransducerMap n
        -> Constraint (ZVar n) (RVar n) IntDouble

getConstraints bigN g arcs ws trans
 = mconcat $  map edgeConstraint arcs
           ++ map weightConstraint ws
 where
  piDiff u v = r1 (Pi v) .-. r1 (Pi u)
  sc     u v = z1 (mkSameCluster u v)

  -- Edge constraints:
  --
  -- For nonfusible edges (u,v), add a constraint
  --    pi(v) - pi(u) >= 1
  -- which is equivalent to
  --    pi(v) > pi(u)
  -- or "v must be scheduled after u"
  -- This will disallow u and v from being in the same cluster, as other
  -- constraints require x(u,v) = 0 can only be true if pi(u) = pi(v).
  --
  -- For fusible edges (u,v)
  --    if they are merged together,    x(u,v) = 0 and pi(v) = pi(u)
  --    otherwise,                      x(u,v) = 1 and pi(v) > pi(u)
  -- This is achieved with the constraint
  --    x(u,v) <= pi(v) - pi(u) <= n * x(u,v)
  --
  --
  -- Note that arcs are reversed in graph, so (v,u) below is actually an edge from u to v.
  edgeConstraint ((v,u), fusible)
   -- The edge must be fusible, the two nodes must have a similar size,
   -- and there can be no other paths between u and v that aren't fusible.
   | fusible && typeComparable g trans u v && noFusionPreventingPath arcs u v
   -- We may want to remove the 'typeComparable' restriction later, and just check
   -- that they have some iteration size, but not necessarily similar.
   -- This would allow fusing @a@ into @c@ in @a = map...; c = cross a b@.
   = let x = sc u v
     in  Between x (piDiff u v) (Z bigN *. x)
     :&& x :<= z1 (C u)

   -- Non-fusible edge, or nodes are different types
   | otherwise
        -- pi(v) - pi(u) >= 1
   =   piDiff u v :>= c1
   :&& z1 (C u)   :== c1


  -- Weights between other nodes:
  --
  -- For any two nodes that may be scheduled together,
  -- we must make sure that if they are together, their pis *must* be the same.
  -- If they are not together, their pis are unconstrained.
  --
  --    -n * x(u,v) <= pi(v) - pi(u) <= n * x(u,v)
  --
  -- That is, if u and v are in the same cluster (x(u,v)=0)
  -- then     pi(v) - pi(u) = 0, or pi(v) = pi(u)
  --
  -- Otherwise, pi(v) - pi(u) has a large enough range to be practically unbounded.
  --
  -- This constraint is not necessary if there is a fusible edge between the two,
  -- as a more restrictive constraint will be added by edgeConstraint.
  --
  weightConstraint (_,u,v)
   -- If there's an edge between the two, don't bother adding this constraint
   | not $ any (\((i,j),_) -> (u,v) == (i,j) || (v,u) == (i,j)) arcs
   = let x = sc u v
     in  Between (Z (-bigN) *. x) (piDiff u v) (Z bigN *. x)
     :&& checkTypes u v

   | otherwise
   = CTrue


  -- If two nodes have different types, but parent transducers with same type,
  -- we may still fuse them together if their parent transducers are fused together
  checkTypes u v
   | Just uT <- nodeType g u
   , Just vT <- nodeType g v
   , uT /= vT
   , Just (u',v') <- trans u v
   =   filtConstraint v' v  u v
   :&& filtConstraint u' u  u v
   :&& filtConstraint u' v' u v

   | otherwise
   = CTrue

  -- c and d can only be fused if a and b are fused
  filtConstraint a b c d
   -- If a and b are the same node, they're already fused!
   | a == b
   = CTrue

   -- Check if it's even possible for a and b to be fused.
   -- There might be a fusion-preventing edge between them.
   | checkFusible a b
   -- If it's possible, constrain (SC a b) <= (SC c d).
   -- This means that if (SC a b) is 1 (unfused), it forces (SC c d) = 1 too.
   = sc a b :<= sc c d

   -- There's a fusion-preventing path between a and b, so they can't possibly be fused.
   -- So c and d won't be fused - let's just set it to 1.
   | otherwise
   = sc c d :== c1

  checkFusible a b
   = any (\(_, i,j) -> (i,j) == (a,b) || (i,j) == (b,a)) ws




-- | Get list of all nodes that might be clustered together,
-- and the weighted benefit of doing so.
clusterings :: (Ord n, Eq t) => [((n,n),Bool)] -> [n] -> Int -> Graph n t -> TransducerMap n -> [(Int, n,n)]
clusterings arcs ns bigN g trans
 = go ns
 where
   -- For some node, find all later nodes of same or similar type, and calculate benefit
   go (u:rest)
    =  [ (w,u,v)
       | v <- rest
       , noFusionPreventingPath arcs u v
       , cmp u v
       , let w = weight u v
       , w > 0]
    ++ go rest
   go []
    = []

   cmp = typeComparable g trans

   -- Simple trick:
   -- if there is an edge between the two,
   --   there will be some cache locality benefit from merging
   -- otherwise,
   --   the only benefit is reducing loop overhead
   --
   -- Another heuristic would be to count nodes with shared parents as having a locality benefit
   weight u v
    -- An edge between them
    | (_:_) <- filter (\((i,j),_) -> (u,v) == (i,j) || (v,u) == (i,j)) arcs
    = bigN * bigN

    -- Share a parent
    | ius <- map (fst.fst) $ filter (\((_,j),_) -> j == u) arcs
    , ivs <- map (fst.fst) $ filter (\((_,j),_) -> j == v) arcs
    , _:_ <- filter (flip elem ius) ivs

    -- Assume that this is for an array.
    = bigN * bigN
    | otherwise
    = 1


-- | Create linear program for graph, and put all the pieces together.
lp :: (Ord n, Eq t) => Graph n t -> TransducerMap n -> Program (ZVar n) (RVar n) IntDouble
lp g trans
 = minimise (gobjective names weights)
            (getConstraints nNodes g arcs weights trans)
            (getBounds names weights)
 where
   g'    = listOfGraph g
   names = map fst $ fst g'
   arcs  =           snd g'

   weights = clusterings arcs names nNodes g trans

   nNodes
     = numNodes g


-- | Find a good clustering for some graph.
-- The output is:
--  (Pi, Type number) -> list of nodes
solve_linear :: (Ord n, Eq t, Show n, Pretty n) => Graph n t -> TransducerMap n -> [[n]]
solve_linear g trans
 = case solve lp's of
   Left  e   -> error (show e)
   Right ass -> Map.elems
              $ fixMap (sub `mappend` ass)
 where
  lp'  = lp g trans
  {- show_lp = CPr.ppr (show.ppr) (show.ppr) -}

  lp'c        = Conv.program lp'
  -- Simplify can return a (Left InfeasibleError) if the program can't be solved,
  -- but we luckily have a proof that the programs we generate will always be feasible.
  Right (sub, lp's) = CSimp.simplify lp'c


  fixMap ass@(Assignment mz _r)
   = reorder ass $ snd $ fillMap $ Map.foldWithKey go (0 :: Int, Map.empty) mz

  go k v (n, m)
   -- SameCluster i j = 0 --> i and j must be fused together
   | SameCluster i j <- k
   , v == 0
   = case (Map.lookup i m, Map.lookup j m) of
     (Just iC, Just jC)
      -> if   iC == jC
         then (n, m)
         else (n, Map.map (\x -> if x == iC then jC else x) m)
     (Just iC, Nothing)
      -> (n, Map.insert j iC m)
     (Nothing, Just jC)
      -> (n, Map.insert i jC m)
     (Nothing, Nothing)
      -> ( n + 1
         , Map.insert i n
         $ Map.insert j n m)

   | otherwise
   = (n, m)

  fillMap (n, m)
   = foldr goFill (n, m) (fst $ listOfGraph g)

  goFill (k,_ty) (n, m)
   | Map.member k m
   = (n, m)
   | otherwise
   = ( n + 1
     , Map.insert k n m)


  reorder ass m
   = Map.fromList
   $ map (reorder' ass)
   $ Map.toList $ invertMap m

  reorder' ass (k,v:vs)
   = let k' = rOf ass (Pi v)
     in  ((truncate k' :: Int, k), v:vs)
  reorder' _ (_, [])
   = error "ddc-core-flow:DDC.Core.Flow.Transform.Rates.Linear: impossible, empty list in inverted map"


#else
solve_linear = error "ddc-core-flow.solve_linear: linear solver was not enabled in build."

#endif
