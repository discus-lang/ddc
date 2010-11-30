{-# LANGUAGE BangPatterns #-}
-- | Bags supply constant time append at the cost of O(n) head and tail.
--   They're equivalent to "append lists". No effort is made to keep
--   the structure balanced. The intent is that you add a pile of things
--   to a bag then convert it to a list or some other stucture.
module Data.Bag
	( Bag
	, empty
	, singleton
	, snoc
	, takeHead
	, append
	, appendList
	, concat
	, map
	, fromList
	, toList)

where
import Data.Monoid
import Prelude			hiding (map, concat)
import qualified Data.List	as List

data Bag a
	= BagNil
	| BagCons  a       (Bag a)
	| BagApp   (Bag a) (Bag a)
	| BagList  [a]     (Bag a)
	deriving (Show)


instance Monoid (Bag a) where
	mempty		= empty
	mappend		= append
	mconcat		= concat
	
	
-- | O(1)
{-# INLINE empty #-}
empty :: Bag a	
empty = BagNil


-- | O(1)
{-# INLINE singleton #-}
singleton :: a -> Bag a
singleton x  = BagCons x BagNil
	

-- | Take the first element from a bag.
snoc ::	Bag a -> (Maybe a, Bag a)
snoc bag
 = case bag of
 	BagNil		
	 -> (Nothing, bag)

	BagCons x bag'	
	 -> (Just x, bag')

	BagApp BagNil b2
	 -> snoc b2
	
	BagApp b1 b2	
	 -> case snoc b1 of
		(mx, b1')	-> (mx, b1' `append` b2)

	BagList [] b2
	 -> snoc b2

	BagList (x:xs) b2
	 -> (Just x, xs `appendList` b2)


takeHead :: Bag a -> Maybe a
takeHead = fst . snoc

-- | O(1)
{-# INLINE append #-}
append :: Bag a -> Bag a -> Bag a
append BagNil b2	= b2
append b1     BagNil	= b1
append b1     b2	= BagApp b1 b2


-- | O(1)
{-# INLINE appendList #-}
appendList :: [a] -> Bag a -> Bag a
appendList xx b1
 = case xx of
 	[]	-> b1
	_	-> BagList xx b1


-- | O(length bb)
concat :: [Bag a] -> Bag a
concat bb
 = case bb of
	[]	-> BagNil
	x : xs	-> x `append` (concat xs)




-- | O(n)
map ::	(a -> b) -> Bag a -> Bag b
map f b1
 = case b1 of
 	BagNil		-> BagNil
	BagCons x b2	-> BagCons (f x) 	   (map f b2)
	BagApp  b1 b2	-> BagApp  (map f b1)      (map f b2)
	BagList xx b2	-> BagList (List.map f xx) (map f b2)


-- | O(1) Convert a list to a Bag.
{-# INLINE fromList #-}
fromList :: [a] -> Bag a
fromList xx
 = case xx of
 	[]		-> BagNil
	_		-> BagList xx BagNil


-- | O(n)				
toList :: Bag a -> [a]
toList	bb
 = go bb [] []
 where 	go bag !rest !acc
	 = case bag of
		BagNil		
		 -> case rest of
			[]		-> reverse acc
			r : rest'	-> go r rest' acc

		BagCons	x  b2	-> go b2  rest        (x:acc)
		BagApp  b1 b2	-> go b1  (b2 : rest) acc
		BagList xs b2   -> go b2  rest        (reverse xs ++ acc)     
