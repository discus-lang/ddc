{-# LANGUAGE TupleSections, FlexibleInstances #-}
module QCGraph where

import Data.List
import Data.Maybe
import Control.Applicative

import Test.QuickCheck
import DDC.Core.Llvm.Convert.Metadata.Graph


instance Arbitrary (UG Int) where
  arbitrary = sized $ \s -> 
    let dom = [0..s `min` magicLimit]
        domG = elements dom
    in  UG . (dom,) . curry . flip elem 
          <$> nub . filter (uncurry (/=)) 
          <$> listOf (lexicoOrder <$> tupleOf domG domG)

-- Unacceptable performance for anything bigger than 5 =(
magicLimit = 3
rootStart  = 42

tupleOf :: Gen a -> Gen b -> Gen (a,b)
tupleOf a b = (,) <$> a <*> b 

lexicoOrder :: Ord a => (a, a) -> (a, a)
lexicoOrder (a , b) | a < b     = (a , b)
                    | otherwise = (b , a) 


-- R+ must: smallest set that contains R and is transitive
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


-- There must always be a transitive orientation if we allow adding edges
--    since the worst case is a complete graph.
prop_orientation_total :: UG Int -> Bool
prop_orientation_total = isJust . transOrientation . minimumCompletion


-- The alias trees generated in the end must not imply some two things
--    are distinct while they alias in the original DDC alias graph.
prop_alias_safety :: UG Int -> Bool
prop_alias_safety g@(UG (d, aliasDDC))
  = null [ (x,y) | x <- d, y <- d
                 , aliasDDC x y
                 , not $ aliasLLVM x y ]
    where trees = snd $ mapAccumL (\r t -> (r+1, anchor r t)) rootStart 
                      $ partitionDAG $ transOrientation' g
          ascendants :: Int -> Tree Int -> [Int]
          ascendants x (Tree (ns, t))
            = let clo = transClosure ns t
              in  filter (clo x) ns
          descendants :: Int -> Tree Int -> [Int]
          descendants x t@(Tree (ns, _))  
            = [ y | y <- ns, x `elem` ascendants y t ]
          aliasLLVM x y 
            =    isNothing (find (\(Tree (ns,t)) -> x `elem` ns && y `elem` ns) trees)
              || any (\t -> y `elem` ((ascendants x t) ++ (descendants x t))) trees

