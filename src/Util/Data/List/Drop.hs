
-- | Dropping / Deleting elements in lists
module Util.Data.List.Drop 
 	( nubF, elemF
	, dropIx
 	, dropWhen)
where

import Data.List

-- | General nub function, using this equality test
nubF ::	(a -> a -> Bool) -> [a] -> [a]
nubF f	xx
 	= nubF' f xx []
	
nubF' f [] acc	= reverse acc
nubF' f (x:xs) acc
	| elemF f x acc	= nubF' f xs acc
	| otherwise	= nubF' f xs (x:acc)


-- | General elem function, using this equality test.
elemF :: (a -> a -> Bool) -> a -> [a] -> Bool
elemF	 f e xs
 = case find (\x -> f e x) xs of
 	Nothing	-> False
	Just x'	-> True
	

-- | Drop out the element with index i from the list.
dropIx :: Int -> [a]	-> [a]
dropIx	i []		= []
dropIx	0 (x:xs)	= xs
dropIx	n (x:xs)	= x : dropIx (n-1) xs


-- | Drop elements when p is true. Dual of filter.
dropWhen :: (a -> Bool) -> [a] 		-> [a]
dropWhen p []		= []
dropWhen p (x:xs)
	| p x 		= dropWhen p xs
	| otherwise	= x : dropWhen p xs

