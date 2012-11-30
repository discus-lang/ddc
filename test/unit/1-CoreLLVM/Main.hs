{-# LANGUAGE TupleSections, FlexibleInstances, TemplateHaskell #-}
module Main where

import Data.List
import Data.Maybe
import Control.Applicative

import Test.QuickCheck
import Test.QuickCheck.All
import DDC.Core.Llvm.Metadata.Graph


main = $(quickCheckAll)

-- Too slow for anything more than 6
magicLimit = 4
rootStart  = 42

instance Arbitrary (UG Int) where
  arbitrary = fmap UG $ arbitraryGraphs

instance Arbitrary (DG Int) where
  arbitrary = fmap DG $ arbitraryGraphs

arbitraryGraphs :: Gen (Dom Int, Rel Int)
arbitraryGraphs 
  = sized $ \s -> 
      let dom = [0..s `min` magicLimit]
          domG = elements dom
      in  (dom,) . curry . flip elem 
            <$> nub . filter (uncurry (/=)) 
            <$> listOf (lexicoOrder <$> tupleOf domG domG)

tupleOf :: Gen a -> Gen b -> Gen (a,b)
tupleOf a b = (,) <$> a <*> b 

lexicoOrder :: Ord a => (a, a) -> (a, a)
lexicoOrder (a , b) | a < b     = (a , b)
                    | otherwise = (b , a) 


-- Give an example of a non-comparability graph
samp_not_comprability_graph :: UG Int -> Bool
samp_not_comprability_graph = isJust . transOrientation

prop_exactly_as_good :: UG Int -> Bool
prop_exactly_as_good ug@(UG (d,g))
  = let completion = length $ toListDG $ transOrientation' ug
        bruteforce = length $ toListDG $ transClosureDG $ bruteforceMinOrientation ug
    in  completion == bruteforce

magic = map transReductionTree . partitionDG . transOrientation'

transOrientation' = fromJust . transOrientation . minimumCompletion
transClosureDG (DG (d,g)) = DG (d, transClosure d g)
transReductionDG (DG (d,g)) = DG (d, transReduction d g) 
transReductionTree (Tree (d,g)) = Tree (d, transReduction d g)
toListDG (DG (d,g)) = toList d g
toListUG (UG (d,g)) = toList d g

-- Transitive closure must be smallest set that contains R and is transitive
--    TODO: find fast way to check "smallest" part
prop_trans_closure_correct :: UG Int -> Bool
prop_trans_closure_correct (UG (d, r))
  = let r'           = toList d r
        clo          = toList d $ transClosure d r
        superset s z = null [ (x,y) | x <- d, y <- d
                                    , (x,y) `elem` z
                                    , not $ (x,y) `elem` s ]
        transitive s = transitiveR d $ fromList s
    in     clo `superset` r'
        && transitive clo


-- The alias trees generated in the end must not imply some two things
--    are distinct while they alias in the original DDC alias graph.
prop_alias_safety :: UG Int -> Bool
prop_alias_safety g@(UG (d, aliasDDC))
  = null [ (x,y) | x <- d, y <- d
                 , aliasDDC x y
                 , not $ aliasLLVM x y ]
    where trees = snd $ mapAccumL (\r t -> (r+1, anchor r t)) rootStart 
                      $ magic g

          ascendants x (Tree (ns, t))
            = let clo = transClosure ns t
              in  filter (clo x) ns
          descendants x t@(Tree (ns, _))  
            = [ y | y <- ns, x `elem` ascendants y t ]

          -- Two things alias when they are either in different trees
          --    or are descendant/ascendant of each other in the same tree
          diffTree x y  = isNothing (find (\(Tree (ns,t)) -> x `elem` ns && y `elem` ns) trees)
          ascdesc  x y  = any (\t -> y `elem` (ascendants x t ++ descendants x t)) trees
          aliasLLVM x y = diffTree x y || ascdesc x y


aliasingAmount :: Eq a => Rel a -> [Tree a] -> Int
aliasingAmount r trees
  = aliasMeasure r $ map listOfNodes trees
    where listOfNodes (Tree (d,_)) = d

-- The alias trees generated induce the least aliasing
--    of any optimal partition of any orientation
--    assuming partition operation is optimal (it's just bruteforce)
prop_alias_optimal :: UG Int -> Bool
prop_alias_optimal ug@(UG (d,rel))
--  = all ((<=) $ aliasingAmount rel $ partitionDG $ minOrientation ug) 
    = all ((<=) $ aliasingAmount rel $ magic ug) 
      $  concatMap withOrientation $ orientations ug
    where withOrientation r = map (aliasMeasure r) 
                                $ filter (isSetOfTrees r) $ partitionings d
          isSetOfTrees r p = all (uncurry isTree) $ map (mkGraph r) p
          mkGraph g nodes = (nodes, fromList [ (x,y) | x <- nodes, y <- nodes, g x y ])

prop_orientation_optimal :: UG Int -> Bool
prop_orientation_optimal ug@(UG (d,_))
  = all ((<=) $ sizeTransClo . relOf $ bruteforceMinOrientation ug) 
      $ map sizeTransClo $ orientations ug
    where sizeTransClo g = length $ toList d $ transClosure d g
          relOf (DG (_,g)) = g

