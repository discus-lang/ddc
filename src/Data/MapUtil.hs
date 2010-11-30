{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Bonus utils for working with `Map`s.
module Data.MapUtil
	( module Data.Map
	, populationCount
	, adjustWithDefault
	, collate)
where
import Data.Map	


-- | Build a map of occurrences of each value in the list.
--   If there are no occurrences then there will be no corresponding element
--   in the resulting list.
populationCount :: Ord a => [a] -> Map a Int
populationCount xx
 	= foldl (flip $ adjustWithDefault (\a -> a + 1) 0)
		empty
		xx


-- | If an element with the given key exists in the map then adjust its
--   value with the provided function, otherwise initilise it to the 
--   given starting value.
adjustWithDefault 
	:: Ord k 
	=> (a -> a) -> a -> k 
	-> Map k a  -> Map k a

adjustWithDefault adjF defaultElem k m	= newMap
  where     
  	oldElem	= findWithDefault defaultElem k m
	newElem	= adjF oldElem

	newMap	= insert k newElem m


-- | Collate a list of tuples, returning a map from the first element to
--   all occurrences of the second element.
collate	:: Ord k
	=> [(k, a)]
	-> Map k [a]
 
collate xs
 = foldr (\(k, a) m -> adjustWithDefault (\x -> a : x) [] k m)
	empty
	xs
