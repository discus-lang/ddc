module DDC.Core.Flow.Transform.Rates.Linear
    (solve_linear, TransducerMap)
 where

import DDC.Core.Flow.Transform.Rates.Graph

import qualified Data.Map  as Map
import           Data.Map    (Map)

import Control.Monad.LPMonad
import Data.LinearProgram
--import Data.LinearProgram.GLPK

import System.IO.Unsafe (unsafePerformIO)

-- | Get parent transducers of two nodes, if exists.
type TransducerMap n = n -> n -> Maybe (n,n)


--
-- | Variable type for linear program
-- Pi i             - {0..} used to show the resulting partition is acyclic:
--
--      A graph is acyclic iff there is a mapping
--          pi : Node -> Int
--      such that \forall (i,j) \in Edge
--          pi(j) > pi(i)
--
-- SameCluster i j  - {0,1} 0 if i and j are fused together
--
-- C n - 0 if node is fused with all its users. Minimising this is good for array contraction.
data GVar n
 = Pi n
 | SameCluster n n
 | C n
 deriving (Eq, Show, Ord)

-- | Canonical form of SameCluster variables.
-- Since we only want to generate one SameCluster variable for each pair, we generate the
-- variable with min variable then max.
mkSameCluster :: Ord n => n -> n -> GVar n
mkSameCluster m n
 = SameCluster (min m n) (max m n)

-- | Minimise objective:
-- \Sigma_i,j Weight(i,j) * SameCluster(i,j)
gobjective :: Ord n => [n] -> [(Int,n,n)] -> LinFunc (GVar n) Int
gobjective ns ws
 =  linCombination
 (  map (\(w,i,j) -> (w, mkSameCluster i j)) ws
 ++ map (\n -> (length ns, C n)) ns)


-- | Set variable types - Pi are floating points, SameCluster and C are "bools" (0 or 1)
setKinds :: Ord n => [n] -> [(Int,n,n)] -> LPM (GVar n) Int ()
setKinds ns ws
 = do   mapM_ setP ns
        mapM_ setW ws
 where
  setP n
   = do varGeq     (Pi n) 0
        setVarKind (Pi n) ContVar
        setVarKind (C  n) BinVar
  setW (_,i,j)
   = do -- varGeq     (SameCluster i j) 0
        setVarKind (mkSameCluster i j) BinVar


-- | Create constraints for edges and weights
addConstraints :: (Ord n, Eq t, Show n)
               => Int -> Graph n t
               -> [((n,n),Bool)]
               -> [(Int,n,n)]
               -> TransducerMap n
               -> LPM (GVar n) Int ()
addConstraints bigN g arcs ws trans
 = do   mapM_ addP arcs
        mapM_ addW ws
 where
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
  addP ((v,u), fusible)
   -- Fusible: edge must be fusible, and they must be same type or have common type transducers
   | fusible && typeComparable g trans u v
   = do let pis = var (Pi v) ^-^ var (Pi u)
        let x   = var (mkSameCluster u v)
        -- x(u,v)         <= pi(v) - pi(u)
        leq x    pis
        -- pi(v) - pi(u)  <= n * x(u,v)
        leq pis (bigN *^ x)
        leq x (var $ C u) 

   -- Non-fusible edge, or nodes are different types
   | otherwise
        -- pi(v) - pi(u) >= 1
   = do geqTo (var (Pi v) ^-^ var (Pi u)) 1
        geqTo (var $ C u) 1
        leqTo (var $ C u) 1


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
  -- as a more restrictive constraint will be added by addP, but it does not
  -- conflict so it can be added anyway.
  --
  addW (_,u,v)
   = do let pis = var (Pi v) ^-^ var (Pi u)
        let x   = var (mkSameCluster u v)
        leq ((-bigN) *^ x) pis
        leq pis  (bigN *^ x)
        checkTypes u v

  -- If two nodes have different types, but parent transducers with same type,
  -- we may still fuse them together if their parent transducers are fused together
  checkTypes u v
   | Just uT <- nodeType g u
   , Just vT <- nodeType g v
   , uT /= vT
   , Just (u',v') <- trans u v
   = do leq' ("Type:" ++ show u ++ ":" ++ show v ++ ":" ++ show u') (var $ mkSameCluster v' v) (var $ mkSameCluster u v)
        leq' ("Type:" ++ show u ++ ":" ++ show v ++ ":" ++ show v') (var $ mkSameCluster u' u) (var $ mkSameCluster u v)
        leq' ("Type:" ++ show u ++ ":" ++ show v) (var $ mkSameCluster u' v') (var $ mkSameCluster u v)
   | otherwise
   = return ()



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
       , noFusionPreventingPath u v
       , cmp u v
       , let w = weight u v
       , w > 0]
    ++ go rest
   go []
    = []

   cmp = typeComparable g trans

   -- \forall paths p from u to v, fusion preventing \not\in p
   noFusionPreventingPath u v
    -- for all paths, for all nodes in path, is fusible
    = all (all snd) (paths u v)

   -- list of all paths from u to v
   paths u v
    | u == v
    = [[]]
    | otherwise
    = let outs = filter (\((i,_j),_f) -> i == u) arcs
      in  concatMap (\((u',j),f) -> map (((u',j),f):) (paths j v)) outs
   
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
    -- TODO check that is array
    = bigN * bigN
    | otherwise
    = 1
   


-- | Create linear program for graph, and put all the pieces together.
lp :: (Ord n, Eq t, Show n) => Graph n t -> TransducerMap n -> LP (GVar n) Int
lp g trans
 = execLPM
 $ do   setDirection Min
        setObjective $ gobjective names weights
        addConstraints nNodes g arcs weights trans
        setKinds names weights
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
solve_linear :: (Ord n, Eq t, Show n) => Graph n t -> TransducerMap n -> Map (Int,Int) [n]
solve_linear g trans
 -- GLPK has a fit if we give it a problem with no constraints.
 -- However, if there are no constraints, it means there are no nodes, or no opportunities for fusion.
 -- Generate a no-fusion clustering.
 | null $ constraints lp'
 = Map.fromList
   [ ((0,n), [k])
   | ((k,_ty),n) <- (fst $ listOfGraph g) `zip` [0..]]

 | otherwise
 = let opts'= mipDefaults { msgLev = MsgOff, brTech = DrTom, btTech = LocBound, cuts = [Cov] }
       res  = unsafePerformIO $ do
                let pre = "lps/lp-" ++ (show $ length $ constraints lp') ++ "-"
                -- writeLP (pre ++ "unopt.lp") lp'unopt
                writeLP (pre ++ "simp.lp") lp'simp
                -- writeFile (pre ++ "prog.p") (prettyProgram p)
                glpSolveVars opts' $ {-trace (pprLP lp')-} lp'
   in  case res of
        (Success, Just (_, m))
         -> fixMap m -- (trace (show m) m)
        _
         -> error (show res)
 where
  lp'simp  = lp g trans
  -- lp'unopt  = lp p g trans False
  lp' = lp'simp

  fixMap m
   = reorder m $ snd $ fill $ Map.foldWithKey go (0, Map.empty) m

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

  fill (n, m)
   = foldr goFill (n, m) (fst $ listOfGraph g)

  goFill (k,_ty) (n, m)
   | Map.member k m
   = (n, m)
   | otherwise
   = ( n + 1
     , Map.insert k n m)


  reorder mOrig m
   = Map.fromList
   $ map (reorder' mOrig)
   $ Map.toList $ invertMap m

  reorder' mOrig (k,v:vs)
   | Just k' <- Map.lookup (Pi v) mOrig
   = ((truncate k', k), v:vs)
   | otherwise
   = error "ddc-core-flow:DDC.Core.Flow.Transform.Rates.Linear: impossible, no Pi value for node"
  reorder' _ (_, [])
   = error "ddc-core-flow:DDC.Core.Flow.Transform.Rates.Linear: impossible, empty list in inverted map"

