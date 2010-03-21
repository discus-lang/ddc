
module Util.Data.List
	( module Data.List
	, module Util.Data.List.Drop
	, module Util.Data.List.Shuffle
	, module Util.Data.List.Select
	, module Util.Data.List.Split
	, test_UtilDataList

	-- contractions
	, catMap, catMapM, catInt
	, isNil
	, walk, walkM, walkM_

	-- Maybified list operators
	, takeInit
	, takeLast
	, takeHead
	, takeTail
	, takeMinimum

	-- maybe
	, listMaybe
	, deadMaybe
	
	-- repetition
	, loopi

	-- update
	, update
	, updateF
	
	-- Map/Accumulate variations.
	, mapAccumLM

	-- Unzip/Map/Zip variations.
	, mapZipped, 	mapZippedM
	
	-- milking
	, milk)	
where
import Data.List
import Util.Data.List.Drop
import Util.Data.List.Shuffle
import Util.Data.List.Select
import Util.Data.List.Split



test_UtilDataList
 = 	test_UtilDataListShuffle
 ++	test_UtilDataListSelect
 ++	test_UtilDataListSplit

-- Simple Contractions ----------------------------------------------------------------------------
catMap		= concatMap

catMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
catMapM f xx
 = do	xxs	<- mapM f xx
	return	$ concat xxs

catInt s xx	= concat $ intersperse s xx


-- walk is map with the args flipped
walk   xx f	= map   f xx
walkM  xx f	= mapM  f xx
walkM_ xx f	= mapM_ f xx
 
isNil :: [a] 	-> Bool
isNil	= null


-- Take versions of prelude ops -------------------------------------------------------------------
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

-- Maybe ------------------------------------------------------------------------------------------
listMaybe :: ([a] -> b) -> [a] -> Maybe b
listMaybe f xx
 = case xx of
 	[]	-> Nothing
	_	-> Just (f xx)

deadMaybe :: Eq a => a -> (a -> b) -> a -> Maybe b
deadMaybe dead f x
	| x == dead	= Nothing
	| otherwise	= Just (f x)


-- Repetition -------------------------------------------------------------------------------------
-- | Repeats some stateful compuation an integral number of times then returns the final state.
loopi :: (state -> state) -> state -> Int -> state
loopi f	s 0	= s
loopi f	s n	= loopi f (f s) (n-1)


-- Update -----------------------------------------------------------------------------------------
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


-- Map/Accumulate variations ----------------------------------------------------------------------
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


-- Unzip/Map/Zip variations -----------------------------------------------------------------------
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
	

-- Milking ----------------------------------------------------------------------------------------
milk 	:: (a -> Maybe ([b], a))	-- keep calling this function while it returns Just
	-> a				-- start with this a.
	-> ([b], a)			-- all the b's that were returned during the calls, and the final a.
	
milk f x	= milk' [] f x
milk' acc f x
 = case f x of
 	Nothing		-> (acc, x)
	Just (ss, x')	-> milk' (acc ++ ss) f x'
		