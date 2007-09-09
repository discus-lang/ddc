
-- Data.Bag 
--	A fast bag of unsorted things.
--
--	For fast insert, union and take1
--	For when you simply don't care about ordering.
--

module Util.Data.Bag
(
	Bag (Nil),

	empty,		
	singleton,
	insert,

	take1,
	mustTake1,

	union,
	unions,
	unionList,
	
	map,
	mapM,

	toList,
	fromList
)

where

import Prelude	hiding (map, mapM)
import qualified Data.List as List

data Bag a
	= Nil
	| Node1  a  	 (Bag a)
	| NodeB  (Bag a) (Bag a)	-- invariant: first bag is never Nil
	| NodeL  [a]     (Bag a)	-- invariant: list is never []
	deriving (Show)
	

-- | O(1)
empty :: 	Bag a	
empty		= Nil


-- | O(1)
singleton :: 	a -> Bag a
singleton x 	= Node1 x Nil

-- | O(1)
insert ::	a -> Bag a -> Bag a
insert x bag	= Node1 x bag
	

take1 ::	Bag a -> (Maybe a, Bag a)
take1 bag
 = case bag of
 	Nil		
	 -> (Nothing, bag)

	Node1 x bag'	
	 -> (Just x, bag')

	NodeB b1 b2	
	 -> let	(mx, b1')	= take1 b1
	    in	(mx, b1' `union` b2)

	NodeL (x:xs) b2
	 -> (Just x, xs `unionList` b2)


mustTake1 ::	Bag a -> (# a, Bag a #)
mustTake1	bag
 = case bag of
 	Nil	-> error "Bag.mustTake1: empty bag"
	
	Node1 x bag'
	 -> (# x, bag' #)
	 
	NodeB b1 b2 
	 -> case mustTake1 b1 of
	 	(# x, b1' #) -> (# x, b1' `union` b2 #)
	 
	NodeL (x:xs) b2
	 -> (# x, xs `unionList` b2 #)

	
-- | O(1)
union ::	Bag a -> Bag a -> Bag a
union b1 b2
 	| Nil		<- b1
	= b2
	
	| Nil		<- b2
	= b1
	
	| otherwise
	= NodeB b1 b2


-- | O(length bb)
unions ::	[Bag a] -> Bag a
unions		bb
 = case bb of
 	[]	-> Nil
	(x:xs)	-> x `union` (unions xs)

	
-- | O(1)
unionList ::	[a] -> Bag a -> Bag a
unionList	xx b1
 = case xx of
 	[]	-> b1
	_	-> NodeL xx b1
		
	
-- | O(n)
map ::		(a -> b) -> Bag a -> Bag b
map	f b1
 = case b1 of
 	Nil		-> Nil
	Node1 x b2	-> Node1 (f x) 		 (map f b2)
	NodeB b1 b2	-> NodeB (map f b1) 	 (map f b2)
	NodeL xx b2	-> NodeL (List.map f xx) (map f b2)
	

-- | O(n)
mapM :: Monad m =>  (a -> m b) -> Bag a -> m (Bag b)
mapM	f b1 
 = case b1 of
 	Nil		-> return Nil

	Node1 x b2	
	 -> do	x'	<- f x
	 	b2'	<- mapM f b2
		return	$ Node1 x' b2'
		
	NodeB b1 b2
	 -> do	b1'	<- mapM f b1
	 	b2'	<- mapM f b2
		return	$  NodeB b1' b2'
		
	NodeL xx b2
	 -> do	xx'	<- sequence $ List.map f xx
	 	b2'	<- mapM f b2
		return	$ NodeL xx' b2'
		

-- | O(n)				
toList :: Bag a -> [a]
toList	bb
 = case bb of
 	Nil		-> []
	Node1 x b2	-> x : toList b2
	NodeB b1 b2	-> toList b1 ++ toList b2
	NodeL xx b2	-> xx ++ toList b2
	

fromList :: [a] -> Bag a
fromList xx
 = case xx of
 	[]		-> Nil
	_		-> NodeL xx Nil
		
	
	
