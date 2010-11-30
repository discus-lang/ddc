{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- | Dropping / Deleting elements in lists
module Data.List.Drop 
 	( elemBy
	, dropIx
 	, dropWhen)
where
import Data.List

-- | General elem function, using this equality test.
elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy f e xs
 = case find (\x -> f e x) xs of
 	Nothing	-> False
	Just _	-> True
	

-- | Drop out the element with index i from the list.
dropIx :: Int -> [a]	-> [a]
dropIx	_ []		= []
dropIx	0 (_:xs)	= xs
dropIx	n (x:xs)	= x : dropIx (n-1) xs


-- | Drop elements when p is true. Dual of filter.
dropWhen :: (a -> Bool) -> [a] 		-> [a]
dropWhen _ []		= []
dropWhen p (x:xs)
	| p x 		= dropWhen p xs
	| otherwise	= x : dropWhen p xs

