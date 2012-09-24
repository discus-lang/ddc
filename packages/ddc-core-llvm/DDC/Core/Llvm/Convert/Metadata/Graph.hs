{-# LANGUAGE TupleSections, FlexibleInstances #-}
-- Manipulate graphs for metadata generation
--  WARNING: everything in here is REALLY SLOW
--
module DDC.Core.Llvm.Convert.Metadata.Graph
       ( Rel(..)
       , fromList, toList
       , allR, differenceR, unionR, composeR, transitiveR
       , transClosure

       , UG(..)
       , DAG(..)
       , transReduction
       , transOrientation,    transOrientation'
       , minimumCompletion
       , partitionDAG

       , Tree(..)
       , sources, anchor )
where

import Data.List          hiding (partition)
import Data.Ord
import Data.Tuple
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Arrow


-- Binary relations -----------------------------------------------------------
type Rel a = a -> a -> Bool
type Dom a = [a]


toList :: Dom a -> Rel a -> [(a, a)]
toList dom r = [ (x, y) | x <- dom, y <- dom, r x y ]

fromList :: Eq a => [(a, a)] -> Rel a
fromList s = \x y -> (x,y) `elem` s

allR :: Eq a => Rel a
allR = (/=)

differenceR :: Rel a -> Rel a -> Rel a
differenceR       f g = \x y -> f x y && not (g x y)

unionR :: Rel a -> Rel a -> Rel a
unionR          f g = \x y -> f x y || g x y

composeR :: Dom a -> Rel a -> Rel a -> Rel a
composeR dom f g = \x y -> or [ f x z && g z y | z <- dom ]

transitiveR :: Dom a -> Rel a -> Bool
transitiveR dom r
 = and [ not (r x y  && r y z && not (r x z)) 
       | x <- dom, y <- dom, z <- dom ]


-- | Find the transitive closure of a binary relation
--      using Floyd-Warshall algorithm
transClosure :: (Eq a) => Dom a -> Rel a -> Rel a
transClosure dom r = fromList $ step dom $ toList dom r
    where step [] es     = es
          step (_:xs) es = step xs 
                              $ nub (es ++ [(a, d) | (a, b) <- es, (c, d) <- es, b == c])


-- | Find the transitive reduction of a finite binary relation
transReduction :: Eq a => Dom a -> Rel a -> Rel a
transReduction dom rel 
  = let composeR' = composeR dom
    in  rel `differenceR` (rel `composeR'` transClosure dom rel)


-- Graphs ---------------------------------------------------------------------
newtype UG  a = UG (Dom a, Rel a)
newtype DAG a = DAG (Dom a, Rel a)

instance Show a => Show (UG a) where
  show (UG (d,r)) = "UG (" ++ (show d) ++ ", " ++ (show $ toList d r) ++ ")"

instance Show a => Show (DAG a) where
  show (DAG (d,r)) = "DAG (" ++ (show d) ++ ", " ++ (show $ toList d r) ++ ")"


-- | Find the transitive orientation of an undirected graph if one exists
--    using exponential-time bruteforce.
--
--    TODO to optimise the current tree representation of metadata in LLVM,
--    we give priority to orientations that are already a tree
--    TODO implement O(n) algorithm
--
transOrientation :: Eq a => UG a -> Maybe (DAG a)
transOrientation (UG (d,g))
 = case toList d g of
        [] -> Just (DAG (d,g))
        edges 
          -> let  -- Treat G as a directed graph. For all subsets S of A (set of arcs),
                  --   reverse the direction of all arcs in S and check if the result
                  --   is transitive.
                  combo k      = filter ((k==) . length) $ subsequences edges
                  choices      = concatMap combo [1..length d]
                  choose c     = g `differenceR` fromList c
                                   `unionR`      fromList (map swap c)
              in  liftM DAG $ liftM (d,) $ find (transitiveR d) $ map choose choices


-- | Find the best transitive orientation possible, adding edges if necessary
transOrientation' :: (Show a, Eq a) => UG a -> DAG a
transOrientation' = fromJust . transOrientation . minimumCompletion


-- | Compute the minimum comparability completion of an undirected graph
--    (i.e. the minimum set of added edges to make the graph
--     transitively orientable)
--    using exponential-time bruteforce (this is NP hard).
--    probably DP-able
--
minimumCompletion :: (Show a, Eq a) => UG a -> UG a
minimumCompletion (UG (d,g))
 = let 
       -- Let U be the set of all possible fill edges. For all subsets
       --   S of U, add S to G and see if the result is trans-orientable.
       u           = toList d $ allR `differenceR` g
       combo k     = filter ((k==) . length) $ subsequences u
       choices     = concatMap combo [0..length u]
       choose c    = g `unionR` fromList c

       -- There always exists a comparability completion for an undirected graph
       --   in the worst case it's the complete version of the graph.
       --   the result is minimum thanks to how `subsequences` and
       --   list comprehensions work.
   in  fromMaybe (error "minimumCompletion: no completion found!") 
                $ liftM UG 
                $ find (isJust . transOrientation . UG) $ map ((d,) . choose) choices


-- Trees ----------------------------------------------------------------------
-- | An inverted tree (with edges going from child to parent)
newtype Tree a = Tree (Dom a, Rel a)

instance Show a => Show (Tree a) where
  show (Tree (d,r)) = "tree (" ++ (show d) ++ ", " ++ (show $ toList d r) ++ ")"


-- | A relation is an (inverted) tree if each node has at most one outgoing arc
isTree :: Dom a -> Rel a -> Bool
isTree dom r 
  = let neighbours x = filter (r x) dom 
    in  all ((<=1) . length . neighbours) dom

sources :: Eq a => a -> Tree a -> [a]
sources x (Tree (d, r)) = [y | y <- d, r y x]


-- | Partition a DAG into the minimum set of (directed) trees
--      once again with bruteforce (this is also NP hard).
--      There always exists a partition, in the worst case 
--      all nodes are disjoint
partitionDAG :: Eq a => DAG a -> [Tree a]
partitionDAG dag@(DAG (_,g))
 = let edgesFor nodes  = [ (x,y) | x <- nodes, y <- nodes, g x y ]    
       mkGraph  nodes  = (nodes, fromList $ edgesFor nodes)
   in map Tree $ fromMaybe (error "partitionDAG: no partition found!") 
               $ find (all $ uncurry isTree) 
               $ map (map mkGraph) 
               $ sortBy (comparing length)
               $ uncurry unmassage
               $ second partitionings 
               $ massage dag

type SubList a   = [a]
type Partitioning a = [SubList a]

-- | Massage the partitioning to fit LLVM metadata trees
--      by putting fully connected nodes in their own trees.
--      TODO: it's better to rank the partitionings on their connectivity
--            and pick the one with the closest to the original.
massage :: Eq a => DAG a -> ([a], Dom a)
massage (DAG (d,g))
  = let connecteds  = filter (\x -> all (liftA2 (||) (g x) (flip g x)) (d \\ [x])) d
    in  (connecteds, d \\ connecteds)

-- | Unmassage the connected nodes back in.
unmassage :: [a] -> [Partitioning a] -> [Partitioning a]
unmassage connecteds = map (connecteds:)

-- | Generate all possible partitions of a list
--    by nondeterministically decide which sublist to add an element to.
partitionings :: Eq a => [a] -> [Partitioning a]
partitionings []     = [[]]
partitionings (x:xs) = concatMap (nondetPut x) $ partitionings xs
  where nondetPut :: a -> Partitioning a -> [Partitioning a]
        nondetPut y []     = [ [[y]] ]
        nondetPut y (l:ls) = let putHere  = (y:l):ls
                                 putLater = map (l:) $ nondetPut y ls
                              in putHere:putLater
                 
        
-- | Enroot a tree with the given root
anchor :: Eq a => a -> Tree a -> Tree a
anchor root (Tree (d,g))
  = let leaves = filter (null . flip filter d . g) d
        arcs   = map (, root) leaves
    in  Tree (root:d, g `unionR` fromList arcs)
