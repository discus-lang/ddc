-----
-- Util.List
--
-- Summary:
--	Random collection of list utils.
--
--
module Util.List
	( module Data.List,

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

	, splitOn, 	splitOns
	, splitWhen,	splitWhens

	, breakOns
	, breakWhens

	, update

	, isNil
	, gather

	, mapT2_1, mapT2_2,
	, mapT3_1, mapT3_2, mapT3_3,

	, partitionFs

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
	, milk)

where

-----
import Data.List
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


-----
splitOn :: 	Eq a 
	=>	a ->	[a] 		-> ([a], [a])
splitOn		c	xx		= splitOn' c xx []

splitOn'	c       []	acc	= (acc, [])
splitOn'	c       (x:xs)	acc
	| c == x			= (acc ++ [x], xs)
	| otherwise			= splitOn' c xs (acc ++ [x]) 


splitOns ::	Eq a => a -> [a] -> [[a]]
splitOns c	= makeSplits (splitOn c)
	

-----
splitWhen ::	(a -> Bool) -> [a]	-> ([a], [a])
splitWhen	p	xx		= splitWhen' p xx []

splitWhen'	p	[]	acc	= (acc, [])
splitWhen'	p	(x:xs)	acc
	| p x				= (acc ++ [x], xs)
	| otherwise			= splitWhen' p xs (acc ++ [x])

splitWhens :: (a -> Bool) -> [a] -> [[a]]
splitWhens f	= makeSplits (splitWhen f)


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
breakOns  c	= makeBreaks (splitOn c)


-- | Break a list into comonents, using this function to choose the separator.
--	The separator element is not returned as part of the components.
--
--	eg: breakWhens (isEven) [1, 5, 2, 5, 9, 5, 7]
--	 -> [[1, 5], [5, 9], [5, 7]]
--
breakWhens :: (a -> Bool) -> [a] -> [[a]]
breakWhens p	= makeBreaks (splitWhen p)


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


-----
partitionFs :: 	[(a -> Bool)] -> [a] -> [[a]]
partitionFs 	fs xx 
 = let
 	bins	= replicate (length fs) []
	bins'	= foldl (partitionFs2 fs) bins xx

   in	map reverse bins'

partitionFs2  fs bins x
 	= partitionFs3 fs [] bins x

partitionFs3  []      bp []      x
	= bp

partitionFs3  (f:fs)  bp (b:bs)  x

	| f x
	= partitionFs3 fs (bp ++ [x:b]) bs x
	
	| otherwise
	= partitionFs3 fs (bp ++ [b])   bs x
	
-----
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
	







