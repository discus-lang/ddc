-- Manipulate graphs for metadata generation
--  WARNING: everything in here is REALLY SLOW
--
module DDC.Core.Llvm.Convert.Metadata.Graph
       ( Rel(..)
       , toList, toListG
       , fromListSym, fromListAntiSym
       , targets, sources

       , UndirectedG(..)
       , DAG(..)
       , transReduction
       , transOrientation,    transOrientation'
       , minimumCompletion,   minimalCompletion
       , partitionDAG

       , Tree(..)
       , rootTree )
where

import Data.List          hiding (partition)
import Data.Tuple
import Data.Maybe
import Control.Monad


-- Binary relations -----------------------------------------------------------
type Rel a = a -> a -> Bool
type Dom a = [a]

toList :: Dom a -> Rel a -> [(a, a)]
toList dom r = [ (x, y) | x <- dom, y <- dom, r x y ]

toListG :: (Dom a, Rel a) -> [(a, a)]
toListG (d,r)  = toList d r

fromListSym     :: Eq a => [(a, a)] -> Rel a
fromListSym     s = \x y -> or [(x,y) `elem` s, (y,x) `elem` s]

fromListAntiSym :: Eq a => [(a, a)] -> Rel a
fromListAntiSym s = \x y -> (x,y) `elem` s

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


transClosure :: Dom a -> Rel a -> Rel a
transClosure dom rel = transclo (length dom)
  where transclo 1 = rel
        transclo n 
          = let transclo' = transclo (n-1)
                step      = \x z -> not $ null $ [y | y <- dom
                                                    , transclo' x y
                                                    , transclo' y z ]
            in  transclo' `unionR` step


-- | Find the transitive reduction of a finite binary relation
transReduction :: Dom a -> Rel a -> Rel a
transReduction dom rel 
  = let composeR' = composeR dom
    in  rel `differenceR` (rel `composeR'` (transClosure dom rel))


-- Graphs ---------------------------------------------------------------------
type UndirectedG a = (Dom a, Rel a)
type DAG         a = (Dom a, Rel a)

isTree :: Dom a -> Rel a -> Bool
isTree dom r 
  = let neighbours x = [ y | y <- dom, r x y ]
    in  and $ map ((<=1) . length . neighbours) dom

targets :: Eq a => a -> DAG a -> [a]
targets x (d, r) = [y | y <- d, r x y]

sources :: Eq a => a -> DAG a -> [a]
sources x (d, r) = [y | y <- d, r y x]


-- | Find the transitive orientation of an undirected graph if one exists
--    using exponential-time bruteforce.
--
--    TODO to optimise the current tree representation of metadata in LLVM,
--    we give priority to orientations that are already a tree
--    TODO implement O(n) algorithm
--
transOrientation :: Eq a => UndirectedG a -> Maybe (DAG a)
transOrientation (d,g)
 = case toList d g of
        [] -> Just (d,g)
        edges 
          -> let  -- Treat G as a directed graph. For all subsets S of A (set of arcs),
                  --   reverse the direction of all arcs in S and check if the result
                  --   is transitive. 
                  combo k      = filter ((k==).length) (subsequences edges)
                  choices      = concat [ combo n | n <- [1..(length d)] ]
                  choose c     = g `differenceR` (fromListAntiSym c) 
                                   `unionR`    (fromListAntiSym $ map swap c)
                  orientations = map choose choices
                  withDom g'   = (d,g')
              in  liftM withDom $ find (transitiveR d) orientations


transOrientation' :: (Show a, Eq a) => UndirectedG a -> DAG a
transOrientation' = fromJust . transOrientation . minimumCompletion

isTransOrientable :: Eq a => UndirectedG a -> Bool
isTransOrientable = isJust . transOrientation


-- | Compute the minimum comparability completion of an undirected graph
--    (i.e. the minimum set of added edges to make the graph
--     transitively orientable)
--    using exponential-time bruteforce (this is NP hard).
-- 
minimumCompletion :: (Show a, Eq a) => UndirectedG a -> UndirectedG a
minimumCompletion (d,g)
 = let 
       -- Let U be the set of all possible fill edges. For all subsets
       --   S of U, add S to G and see if the result is trans-orientable.
       u           = toList d $ allR `differenceR` g
       combo k     = filter ((k==).length) (subsequences u)
       choices     = concat [ combo n | n <- [0..(length u)] ]
       choose c    = g `unionR` (fromListAntiSym c)
       completions = map choose choices
       withDom g'  = (d,g')

       -- There always exists a comparability completion for an undirected graph
       --   in the worst case it's the complete version of the graph.
       --   the result is minimum thanks to how `subsequences` and
       --   list comprehensions work.
   in  case find (isTransOrientable . withDom) completions of
            Just comp -> (d, comp)
            Nothing   -> error "minimumCompletion: no completion found! " $ show $ map (toList d) completions


-- | Find the minimal comparability completion of an undirected graph
--      (the approximation of a minimum completion)
--
minimalCompletion :: UndirectedG a -> UndirectedG a
minimalCompletion = undefined
   

type Tree a = (Dom a, Rel a)

-- | Partition a DAG into the minimum set of (directed) trees
--      once again with bruteforce (this is also NP hard).
--      There always exists a partition, in the worst case 
--      all nodes are disjoint
partitionDAG :: Eq a => DAG a -> [Tree a]
partitionDAG (d,g)
 = let nodePartitions  = partitions d       
       edgesFor nodes  = [ (x,y) | x <- nodes, y <- nodes, g x y ]    
       mkDAG nodes     = (nodes, fromListAntiSym $ edgesFor nodes)
       mkDAGs par      = map mkDAG par       
       allTrees dags   = and $ map (\(d',g') -> isTree d' g') dags
   in  case find allTrees $ map mkDAGs nodePartitions of
            Just p  -> p
            Nothing -> error "partitionDAG: no partition found! Fix your bugs!"

partitions :: Eq a => [a] -> [[[a]]]
partitions nodes = concat [ npartition n | n <- [1..m] ]
  where m             = length nodes
        combo k xs    = filter ((k==).length) (subsequences xs)
        npartition k  = concatMap match $ combo k nodes
        match      cs = [ [cs] ++ [cs'] 
                        | cs' <- combo (m - (length cs)) (nodes \\ cs) ] 

rootTree :: Eq a => a -> Tree a -> Tree a
rootTree root (d,g)
  = let leaves = filter (\x -> null [y | y <- d, g x y]) d
        arcs   = [ (x, root) | x <- leaves ]
    in  (root:d, g `unionR` (fromListAntiSym arcs))
