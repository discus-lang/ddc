{-# LANGUAGE TupleSections, FlexibleInstances, TemplateHaskell #-}
module Main where

import Data.List 
import Data.Ord
import Data.Tuple
import Data.Maybe
import Control.Monad
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
  where tupleOf :: Gen a -> Gen b -> Gen (a,b)
        tupleOf a b = (,) <$> a <*> b         
        lexicoOrder :: Ord a => (a, a) -> (a, a)
        lexicoOrder (a , b) | a < b     = (a , b)
                            | otherwise = (b , a) 


-- Properties -----------------------------------------------------------------

-- The alias trees generated in the end must not imply some two things
--    are distinct while they alias in the original DDC alias graph.
prop_alias_safety :: UG Int -> Bool
prop_alias_safety g@(UG (d, aliasDDC))
  = null [ (x,y) | x <- d, y <- d
                 , aliasDDC x y
                 , not $ aliasLLVM x y ]
    where aliasLLVM x y = diffTree x y || ascdesc x y

          -- Two things alias when they are either in different trees
          --    or are descendant/ascendant of each other in the same tree
          diffTree x y  = isNothing (find (\(Tree (ns,t)) -> x `elem` ns && y `elem` ns) trees)
          ascdesc  x y  = any (\t -> y `elem` (ascendants x t ++ descendants x t)) trees
          ascendants x (Tree (ns, t))    = let clo = transClosure ns t 
                                           in  filter (clo x) ns
          descendants x t@(Tree (ns, _)) = [ y | y <- ns, x `elem` ascendants y t ]
          trees = ddcConversion g

-- Current NOT TRUE for non-comparability graphs
--
prop_alias_optimal :: UG Int -> Bool
prop_alias_optimal ug@(UG (_, f))
  = aliasing (ddcConversion ug) <= aliasing (optimalConversion ug)
  where aliasing :: [Tree Int] -> Int
        aliasing ts = aliasMeasure f $ map (\(Tree (d,_)) -> d) ts


-- Conversion -----------------------------------------------------------------

ddcConversion :: UG Int -> [Tree Int]
ddcConversion ug = snd $ mapAccumL (\r t -> (r+1, anchor r t)) rootStart 
                       $ ddcPartition
                       $ transOrient ug
  where ddcPartition = map transReductionTree . partitionDG
        transReductionTree (Tree (d,g)) = Tree (d, transReduction d g)                        


optimalConversion :: UG Int -> [Tree Int]
optimalConversion ug = snd $ mapAccumL (\r t -> (r+1, anchor r t)) rootStart 
                           $ ddcPartition
                           $ bruteforceMinOrientation ug
  where ddcPartition = map transReductionTree . partitionDG
        transReductionTree (Tree (d,g)) = Tree (d, transReduction d g) 

bruteforceMinOrientation :: (Show a, Ord a) => UG a -> DG a
bruteforceMinOrientation ug@(UG (d, _))
  = let minTransClo : _ = sortBy (comparing $ transCloSize d) $ orientations ug
     in DG (d, minTransClo)
  where orientations (UG (d,g)) = case toList d g of
          []    -> [g]
          edges -> let combo k      = filter ((k==) . length) $ subsequences edges
                       choices      = concatMap combo [0..length d]
                       choose c     = g `differenceR` fromList c
                                        `unionR`      fromList (map swap c)
                    in map choose choices     

size :: Dom a -> Rel a -> Int
size d r = length $ toList d r

composeR :: Dom a -> Rel a -> Rel a -> Rel a
composeR dom f g = \x y -> or [ f x z && g z y | z <- dom ]

unionR :: Rel a -> Rel a -> Rel a
unionR f g = \x y -> f x y || g x y

differenceR :: Rel a -> Rel a -> Rel a
differenceR     f g = \x y -> f x y && not (g x y)

transReduction :: Eq a => Dom a -> Rel a -> Rel a
transReduction dom rel 
  = let composeR' = composeR dom
    in  rel `differenceR` (rel `composeR'` transClosure dom rel)

transCloSize :: (Eq a) => Dom a -> Rel a -> Int
transCloSize d r = size d $ transClosure d r

