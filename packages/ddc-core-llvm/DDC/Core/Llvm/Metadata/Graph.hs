-- Manipulate graphs for metadata generation
{-# LANGUAGE TupleSections #-}
module DDC.Core.Llvm.Metadata.Graph
       ( -- * Graphs and Trees for TBAA metadata
         UG(..), DG(..)
       , orientUG, partitionDG
       , Tree(..)
       , sources, anchor 

         -- * Quickcheck Testing ONLY
       , Dom, Rel
       , fromList, toList
       , transClosure, transOrient
       , aliasMeasure, isTree )
where
import Data.List 
import Data.Ord
import Data.Maybe


-- Binary relations -----------------------------------------------------------
-- | A binary relation.
type Rel a = a -> a -> Bool
type Dom a = [a]


-- | Convert a relation.
toList :: Dom a -> Rel a -> [(a, a)]
toList dom r = [ (x, y) | x <- dom, y <- dom, r x y ]


-- | Convert a list to a relation.
fromList :: Eq a => [(a, a)] -> Rel a
fromList s = \x y -> (x,y) `elem` s


-- | Union two relations.
unionR :: Rel a -> Rel a -> Rel a
unionR f g = \x y -> f x y || g x y


-- | Find the transitive closure of a binary relation
--      using Floyd-Warshall algorithm
transClosure :: (Eq a) => Dom a -> Rel a -> Rel a
transClosure dom r = fromList $ step dom $ toList dom r
    where step [] es     = es
          step (_:xs) es = step xs 
                          $ nub (es ++ [(a, d) 
                                | (a, b) <- es
                                , (c, d) <- es
                                , b == c])


-- Graphs ---------------------------------------------------------------------
-- | An undirected graph.
newtype UG  a = UG (Dom a, Rel a)

-- | A directed graph.
newtype DG  a = DG (Dom a, Rel a)

instance Show a => Show (UG a) where
  show (UG (d,r)) = "UG (" ++ (show d) ++ ", fromList " ++ (show $ toList d r) ++ ")"

instance Show a => Show (DG a) where
  show (DG (d,r)) = "DG (" ++ (show d) ++ ", fromList " ++ (show $ toList d r) ++ ")"

instance Show a => Eq (DG a) where
  a == b = show a == show b 

neighbourUG :: Rel a -> a -> a -> Bool
neighbourUG f v x = f v x  || f x v


-- | A partition (class) of vertices
type Class a = [a]


-- | Enforce an ordering on the relation of an undirected graph
forceOrder :: Ord a => Class a -> Rel a -> Rel a 
forceOrder ordering f 
  = let index = fromJust . (flip elemIndex ordering) 
    in  \x y -> neighbourUG f x y && index x < index y


-- | Set of vertices is not a singleton or empty set
nonSingleton :: Class a -> Bool
nonSingleton []  = False
nonSingleton [_] = False
nonSingleton _   = True

                      
-- | Use lexicographic breadth-first search on an undirected graph to produce an ordering of the vertices
--              
lexBFS :: (Show a, Ord a) => UG a -> Class a
lexBFS (UG (vertices, f)) = refine [] [vertices]
  where refine acc classes
          | any nonSingleton classes = pivot acc classes
          | otherwise                = concat classes ++ acc

        pivot acc ([vertex]:classes)    = refine (vertex:acc) $ classes      `splitAllOn` vertex
        pivot acc ((vertex:vs):classes) = refine (vertex:acc) $ (vs:classes) `splitAllOn` vertex
        pivot _   _                     = error "impossible!"

        splitAllOn [] _ = []
        splitAllOn (cl:classes) vertex
          | (neighbours, nonneighbours) <- partition (neighbourUG f vertex) cl
          , all (not . null) [neighbours, nonneighbours]
          = nonneighbours : neighbours : (classes `splitAllOn` vertex)
          | otherwise 
          = cl                         : (classes `splitAllOn` vertex)


-- | Transitively orient an undireted graph
--
--      Using the algorithm from
--      "Lex-BFS and partition refinement, with applications to transitive orientation, interval 
--      graph recognition and consecutive ones testing", R. McConnell et al 2000
--
--      In the case where the transitive orientation does not exist, it simply gives some orientation
--
--      note: gave up on modular decomposition, this approach has very slightly worse time
--            complexity but much simpler
--   
transOrient :: (Show a, Ord a) => UG a -> DG a
transOrient g@(UG (vertices, f))
  = let vertices' = refine $ [(lexBFS g, maxBound)]
    in  DG (vertices, forceOrder vertices' f)
  where refine classes 
          | any nonSingleton $ map fst classes
          = let (before, after) = partition (\(c,lastused) -> length c > lastused `div` 2) classes
            in  refine (splitOthers before after)
          | otherwise = concatMap fst classes
        
        -- Split all other classes with respect to each member of a pivot class
        splitOthers before [] = splitLargest (largestClass before) before
        splitOthers before ((pivot,_):after)
          =    foldl' (split True) before pivot 
            ++ [(pivot, length pivot)] 
            ++ foldl' (split False) after pivot

        -- Split a class cl with regard to some vertex
        split _ [] _ = []
        split isBefore (cl:classes) vertex
          | (neighbours, nonneighbours) <- partition (neighbourUG f vertex) $ fst cl
          , all (not . null) [neighbours, nonneighbours]
          = let lastused = snd cl
            in  if   isBefore 
                then (nonneighbours, lastused) : (neighbours,    lastused) : (split isBefore classes vertex)
                else (neighbours,    lastused) : (nonneighbours, lastused) : (split isBefore classes vertex)
          | otherwise = cl:classes

        -- Split the largest class by the last vertex in the class found by lexBFS
        splitLargest _ [] = []
        splitLargest cl ((cs, lastused):css)
          | cl == cs  = (tail cs, lastused) : ([head cs], maxBound) : css
          | otherwise = (cs, lastused) : (splitLargest cl css)

        largestClass []      = []
        largestClass classes = maximumBy (comparing length) $ map fst classes
         

orientUG :: (Show a, Ord a) => UG a -> DG a
orientUG = transOrient


-- | A vertex partitioning of a graph.
type Partitioning a = [Class a]


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


-- | Calculate the aliasing induced by a set of trees this includes aliasing
--   within each of the trees and aliasing among trees.
---
--   ISSUE #298: Need a more efficient way to compute the
--     aliasing measure. Currently O(|V|^5)
--
aliasMeasure :: Eq a => Rel a -> Partitioning a -> Int
aliasMeasure g p
 = (outerAliasing $ map length p) + (sum $ map innerAliasing p)
    where innerAliasing t = length $ toList t $ transClosure t g
          outerAliasing (l:ls) = l * (sum ls) + outerAliasing ls
          outerAliasing []     = 0    


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


-- | Get the sources of a tree.
sources :: Eq a => a -> Tree a -> [a]
sources x (Tree (d, r)) = [y | y <- d, r y x]


-- | Partition a DG into the minimum set of (directed) trees
--
partitionDG :: Eq a => DG a -> [Tree a]
partitionDG (DG (d,g))
 = let mkGraph  g' nodes = (nodes, fromList [ (x,y) | x <- nodes, y <- nodes, g' x y ])
   in map Tree $ fromMaybe (error "partitionDG: no partition found!") 
               $ find (all $ uncurry isTree) 
               $ map (map (mkGraph g)) 
               $ sortBy (comparing (aliasMeasure g))
               $ partitionings d

                    
-- | Enroot a tree with the given root.
anchor :: Eq a => a -> Tree a -> Tree a
anchor root (Tree (d,g))
  = let leaves = filter (null . flip filter d . g) d
        arcs   = map (, root) leaves
    in  Tree (root:d, g `unionR` fromList arcs)

