-----
-- Util.List
--
-- Summary:
--	Random collection of list utils.
--
--
module Util.List
	( module Data.List

	-- concat / map
	, catMap
	, catMapM
	, catInt
	
	-- 
	, walk, walkM, walkM_
	
	, dropIx
	, dropWhen
	, eachAndOthers

	, elemF
	, foldlR
	, foldlR_
	, interslurp
	, lookupF
	, loopi
	, nubF
	, rotate
	, remove
	, sequenceF
	, sequenceFF
	

	-- splitting
	, splitWhenLeft, chopWhenLeft
	, splitOnLeft, chopOnLeft
	, splitWhenRight, chopWhenRight
	, splitOnRight, chopOnRight

	, breakOns
	, breakWhens

	, update

	, isNil
	, gather

	, mapT2_1, mapT2_2
	, mapT3_1, mapT3_2, mapT3_3

	-- Maybified list operators
	--	Use these to avoid nasty, impossible to find exceptions like (head [])
	, takeInit
	, takeLast
	, takeHead
	, takeTail
	, takeMinimum

	, listMaybe
	, deadMaybe

	-- Map/Accumulate variations.
	, mapAccumLM

	-- Unzip/Map/Zip variations.
	, mapZipped, 	mapZippedM
	
	-- milking
	, milk
	
	-- sorting
	, partitionFs
	, partitionFsSort)
where

-----
import Data.List
import Data.Maybe

import qualified Data.Map	as Map
import Util.Tunnel

catMap		= concatMap
catInt s xx	= concat $ intersperse s xx

catMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
catMapM f xx
 = do
	xxs	<- mapM f xx
	return	$ concat xxs


walk   xx f	= map   f xx
walkM  xx f	= mapM  f xx
walkM_ xx f	= mapM_ f xx
 	

-----
-- dropIx
--	Drops out element with index i from the list.
--
dropIx :: Int -> [a]	-> [a]
dropIx	 i	[]	= []
dropIx	0	(x:xs)	= xs
dropIx	n	(x:xs)	= x : dropIx (n-1) xs


-----
-- dropWhen
--	Drops out elements of a list x, where p(x) is true.
--
dropWhen :: (a -> Bool) -> [a] 		-> [a]
dropWhen    p 		   []		= []
dropWhen    p 		   (x:xs)
	| p x 				= dropWhen p xs
	| otherwise			= x : dropWhen p xs


-----
-- eachAndOthers 
--	Takes a list L, returns a list of pairs (x, xs) where x is an element of L
--	and xs is a list of the the other elements of L.
--
--	eachAndOthers [1, 2, 3, 4]
--		= [(1, [2, 3, 4]), (2, [1, 3, 4]), (3, [1, 2, 4]), (4, [1, 2, 3])]
--
--
eachAndOthers :: [a] 		-> [(a, [a])]
eachAndOthers	 xx		= eachAndOthers' xx []

eachAndOthers'	 []	prev	= []
eachAndOthers'	 (x:xs) prev 	= (x, prev ++ xs) : eachAndOthers' xs (prev ++ [x])


-----
elemF  ::	(a -> a -> Bool) ->	a -> [a]	-> Bool
elemF		f			e    xs
 = case find (\x -> f e x) xs of
 	Nothing	-> False
	Just x'	-> True


-----
foldlR  :: (s -> i -> (s, o)) -> s -> [i] -> (s, [o])
foldlR	   f s xs	 = foldlR' f s xs []

foldlR'    f s []     ys = (s, ys)
foldlR'	   f s (x:xs) ys = foldlR' f s' xs (y:ys)
 where
 	(s', y)		= f s x

-----
foldlR_ :: (s -> i -> (s, o)) -> s -> [i] -> s

foldlR_	   f s []	= s
foldlR_	   f s (x:xs)	= foldlR_ f s' xs
 where
 	(s', y)		= f s x


-----
-- interslurp
--	Inverse of intersperse (mostly)
--	Takes every second (internal) element of a list
--
--	interslurp [1, 2, 3, 4, 5, 6] = [2, 4]
--
interslurp ::	[a] 		-> [a]
interslurp   	[]		= []
interslurp	(a:[])		= []
interslurp	(a:b:[])	= []
interslurp	(a:b:xs)  	= b : interslurp xs


-----
lookupF	::      (a -> a -> Bool) ->	a -> [(a, b)] 	-> Maybe b
lookupF		f			a    []		= Nothing
lookupF		f			a    ((k,d):xs)
 | f a k	= Just d
 | otherwise	= lookupF f a xs


-----
-- loopi
--	Repeats some stateful compuation an integral number of times.
--	Returns final state.
--
loopi ::	(state -> state) -> state -> Int -> state
loopi		f	s 	0	= s
loopi		f	s	n	= loopi f (f s) (n-1)


-----
nubF ::		(a -> a -> Bool) ->	[a]		-> [a]
nubF		f			xx
 	= nubF' f xx []
	
nubF'		f []	 acc	= reverse acc
nubF'		f (x:xs) acc
	| elemF f x acc	= nubF' f xs acc
	| otherwise	= nubF' f xs (x:acc)


-----
rotate :: Int -> [a] 	-> [a]
rotate    n      xx 	
 | n == 0		= xx
 | n >  0		= take (length xx) $ drop n $ cycle xx
 | n <  0		= take (length xx) $ drop (length xx + n) $ cycle xx



-----
remove  :: (a -> Bool) -> [a] -> Maybe (a, [a])
remove     f               xx = remove' f [] xx

remove' :: (a -> Bool) -> [a] -> [a] 	-> Maybe (a, [a])
remove'    f	         prev   []	= Nothing
remove'    f     	 prev   (x:xs)  
 | f x					= Just (x, prev ++ xs)
 | otherwise				= remove' f (x:prev) xs


-----
sequenceF :: 	[(a -> a)] -> 	a 	-> a
sequenceF	[]		a	= a
sequenceF 	(f:fs)		a 	= sequenceF fs (f a)

sequenceFF ::	a ->		[(a -> a)]	-> a
sequenceFF	a		[]		= a
sequenceFF	a		(f:fs)		= sequenceF fs (f a)


-- Splitting  --------------------------------------------------------------------------------------


-- Split Left --------

-- | Split a list on the left of the first element where this predicate matches.
splitWhenLeft ::	(a -> Bool) -> [a]	-> ([a], [a])
splitWhenLeft	p	xx		= splitWhenLeft' p xx []

splitWhenLeft'	p	[]	acc	= (acc, [])
splitWhenLeft'	p	(x:xs)	acc
	| p x				= (acc, x : xs)
	| otherwise			= splitWhenLeft' p xs (acc ++ [x])

chopWhenLeft :: (a -> Bool) -> [a] -> [[a]]
chopWhenLeft f xx	= makeSplits (splitWhenLeft f) xx

-- | Split a list on the left of the first occurance of this element.
--	eg splitLeft '.' "abc.def"  => ("abc", ".def")

splitOnLeft :: Eq a => a -> [a] -> ([a], [a])
splitOnLeft c xx	= splitWhenLeft (== c) xx

-- | Split a list on the left of all occurances of this element.
--	eg chopBefore '." "abc.def.ghi.jkl"	=> ["abc", ".def", ".ghi", ".jkl"]

chopOnLeft :: Eq a => a -> [a] -> [[a]]
chopOnLeft c xx		= makeSplits (splitOnLeft c) xx


-- Split Right ---------

-- | Split a list on the right of the first element where this predicate matches
--
splitWhenRight ::	(a -> Bool) -> [a]	-> ([a], [a])
splitWhenRight	p	xx		= splitWhenRight' p xx []

splitWhenRight'	p	[]	acc	= (acc, [])
splitWhenRight'	p	(x:xs)	acc
	| p x				= (acc ++ [x], xs)
	| otherwise			= splitWhenRight' p xs (acc ++ [x])

chopWhenRight :: (a -> Bool) -> [a] -> [[a]]
chopWhenRight f	xx	= makeSplits (splitWhenRight f) xx


-- | Split a list on the right of the first occurance of this element.
--	eg splitRight '.' "abc.def" => ("abc.", "def")

splitOnRight :: Eq a => a -> [a] -> ([a], [a])
splitOnRight c xx	= splitWhenRight (== c) xx

-- | Split a list on the right of all occurances of this element.
--	eg chopRight '.' "abc.def.ghi.jkl" 	=> ["abc.", "def.", "ghi.", "jkl"]

chopOnRight :: Eq a => a -> [a] -> [[a]]
chopOnRight c xx	= makeSplits (splitOnRight c) xx


-----
-- Split Functions
--
makeSplits 
	:: ([a] -> ([a], [a])) 
	-> ([a] -> [[a]])

makeSplits	splitFunc xx
 = case splitFunc xx of
 	(chunk, [])			-> [chunk]
	(chunk, more)			-> chunk : makeSplits splitFunc more
	



-- | Make a breaks function from this split function
makeBreaks
	:: ([a] -> ([a], [a])) -> ([a] -> [[a]])

makeBreaks	splitFunc xx
 = let 	parts 		= makeSplits splitFunc xx
	firstParts'	= map init $ init parts
   in 	firstParts' ++ [last parts]
	

-- | Break a list into components, using this element as the separator.
--	The element is not returned as part of the components.
--
--	eg:  breakOns '.' "rabbit.marmot.lemur"   
--	  -> ["rabbit", "marmot", "lemur"]
--
breakOns :: Eq a => a -> [a] -> [[a]]
breakOns  c	= makeBreaks (splitOnRight c)


-- | Break a list into comonents, using this function to choose the separator.
--	The separator element is not returned as part of the components.
--
--	eg: breakWhens (isEven) [1, 5, 2, 5, 9, 5, 7]
--	 -> [[1, 5], [5, 9], [5, 7]]
--
breakWhens :: (a -> Bool) -> [a] -> [[a]]
breakWhens p	= makeBreaks (splitWhenRight p)


-----
-- Update Functions
--
update	:: Eq a 
	=> a -> b -> [(a, b)] -> [(a, b)]
update	key elem	[]		= []
update  key elem 	((k, e):xs)
	| k == key	= (k, elem) : update key elem xs
	| otherwise	= (k, e)    : update key elem xs

-----
updateF :: Eq k
	=> k -> (e -> e) -> [(k, e)] -> [(k, e)]
updateF key f		[]		= []
updateF key f		((k, e):xs)	
	| k == key	= (k, f e)	: updateF key f xs
	| otherwise	= (k, e)	: updateF key f xs


-----
-- There's a Haskell function called simply 'null'
-- but simple boolean tests should probably be called isSomething
--
isNil :: [a] 	-> Bool
isNil    []	= True
isNil	 xx	= False


-----
-- gather
--	gather [(0, 1), (0, 2), (3, 2), (4, 5), (3, 1)] = [(0, [1, 2]), (3, [2, 1]), (4, [5])]
--
--
gather :: Ord a => [(a, b)] -> [(a, [b])]
gather	xx	= Map.toList m
 where
 	m	= foldr (\(k, v) m -> 
			 	Map.insertWith 
					(\x xs -> x ++ xs) 
					k [v] m) 
			Map.empty 
			xx
	

-----
mapT2_1 = mapUpdateTl tl2_1

mapT2_2 :: (b -> c) -> [(a, b)] -> [(a, c)]
mapT2_2 f xx
 = case xx of 
 	[]		-> []
	((a, b):xs)	-> (a, f b) : mapT2_2 f xs
	 

mapT3_1	= mapUpdateTl tl3_1 
mapT3_2 = mapUpdateTl tl3_2 
mapT3_3 = mapUpdateTl tl3_3


	
-- List Operators --------------------------------------------------------------
takeInit ::	[a] -> Maybe [a]
takeInit	xx
 = case xx of
 	[]	-> Nothing
	_	-> Just (init xx)


takeLast ::	[a] -> Maybe a
takeLast	xx
 = case xx of
 	[]	-> Nothing
	_	-> Just (last xx)

	
takeHead ::	[a] -> Maybe a
takeHead	xx
 = case xx of
 	[]	-> Nothing
	_	-> Just (head xx)	 


takeTail ::	[a] -> Maybe [a]
takeTail	xx
 = case xx of
 	[]	-> Nothing
	_	-> Just (tail xx)


takeMinimum 
	:: Ord a 
	=> [a] -> Maybe a

takeMinimum	xx
 = case xx of
 	[]	-> Nothing
	_	-> Just (minimum xx)

	
listMaybe :: ([a] -> b) -> [a] -> Maybe b
listMaybe f xx
 = case xx of
 	[]	-> Nothing
	_	-> Just (f xx)

deadMaybe :: Eq a => a -> (a -> b) -> a -> Maybe b
deadMaybe dead f x
	| x == dead	= Nothing
	| otherwise	= Just (f x)
	
-----
-- Map/Accumulate variations.
--

mapAccumLM 
	:: Monad m
	=> (acc -> x -> m (acc, y)) -> acc -> [x] -> m (acc, [y])

mapAccumLM f acc xx
 	= mapAccumLM' f acc xx []

mapAccumLM' f acc []  yy   
 	= return (acc, yy)
 
mapAccumLM' f acc (x:xs) yy
 = do	(acc', y)	<- f acc x
 	mapAccumLM' f acc' xs (yy ++ [y])


-----
-- Unzip/Map/Zip variations.
-- 
mapZipped 
	:: (a -> a2) 
	-> (b -> b2) 
	-> [(a, b)] -> [(a2, b2)]

mapZipped    f g xx
 = let	(as, bs)	= unzip xx
 	as'		= map f as
	bs'		= map g bs
   in	zip as' bs'


mapZippedM 
	:: Monad m
	=> (a -> m a2)
	-> (b -> m b2)
	-> [(a, b)] -> m [(a2, b2)]

mapZippedM f g xx
 = do	let (as, bs)	= unzip xx
 	as'		<- mapM f as
	bs'		<- mapM g bs
	return		$ zip as' bs'
	

-----
-- milking
--
milk 	:: (a -> Maybe ([b], a))	-- keep calling this function while it returns Just
	-> a				-- start with this a.
	-> ([b], a)			-- all the b's that were returned during the calls, and the final a.
	
milk f x	= milk' [] f x
milk' acc f x
 = case f x of
 	Nothing		-> (acc, x)
	Just (ss, x')	-> milk' (acc ++ ss) f x'
	



-- Sorting ---------------------------------------------------------------------

-- | Separate out this list into bins. 
--   An element goes in the bin if it matches the corresponding predicate.

partitionFs :: 	[(a -> Bool)] -> [a] -> ([[a]], [a])
partitionFs 	fs xx 
 = let	(bins, floor)	= mapAccumL (partitionFs1 fs) (replicate (length fs) []) xx
   in	( map reverse bins
        , catMaybes floor)
	
-- | Place an element on the head of the bin which matches the corresponding predicate
--	If no bins match the element goes on the floor.

partitionFs1 :: [(a -> Bool)] -> [[a]] -> a -> ([[a]], Maybe a)

partitionFs1 fs bins x 
	= partitionFs1' fs [] bins x

-- no predicates matched, drop the element on the floor
partitionFs1'  []      binPrev []	x
	= (binPrev, Just x)

-- ran out of functions before we ran out of bins
--	just say we couldn't match anything
partitionFs1'	[]	binPrev binsRest 	x
	= ( binPrev ++ binsRest
	  , Just x)	

-- ran out of bins before we ran out of functions
--	just make a new bin and carry on
partitionFs1'  ff  	binPrev []  	x
	= partitionFs1' ff binPrev [[]] x

partitionFs1'  (f:fs)  binPrev (bin:binRest)  x

	-- predicate matched, place the element in the current bin
	| f x
	= ( binPrev ++ [x:bin] ++ binRest
	  , Nothing)
	
	-- predicate failed, move on to the next bin.
	| otherwise
	= partitionFs1' fs (binPrev ++ [bin]) binRest x


-- | Do a partitionFs then concat the results together into a single list.
partitionFsSort :: [(a -> Bool)] -> [a] -> [a]
partitionFsSort fs xx
 = let	(bins, floor)	= partitionFs fs xx
   in	(concat bins ++ floor)
	









