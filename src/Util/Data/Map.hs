
module Util.Data.Map
	( module Data.Map
	, populationCount
	, adjustWithDefault
	, gather)

where

import Data.Map	

-----
populationCount :: Ord a => [a] -> Map a Int
populationCount xx
 	= foldl (flip $ adjustWithDefault (\a -> a + 1) 0)
		empty
		xx

-----
adjustWithDefault 
 :: 	Ord k 
 => 	(a -> a) -> a -> k 
 -> 	Map k a  -> Map k a

adjustWithDefault adjF defaultElem k m	= newMap
  where     
  	oldElem	= findWithDefault defaultElem k m
	newElem	= adjF oldElem

	newMap	= insert k newElem m

-----
gather
 ::	Ord k
 =>	[(k, a)] -> Map k [a]
 
gather xs
	= foldr (\(k, a) m -> adjustWithDefault (\x -> a : x) [] k m)
		empty
		xs

