
-- | Bonus utils for working with `Either`s.
module Data.EitherUtil
	( partitionEither)
--	, catLeftOrError)
where


-- | Partition a list of `Eithers` into its left and right elements.
partitionEither :: [Either a b] -> ([a], [b])
partitionEither	ee
 = let	(accL, accR)	= partitionEither' [] [] ee
   in	(reverse accL, reverse accR)

partitionEither' accL accR ee
 = case ee of
 	Left  a : es	-> partitionEither' (a : accL) accR       es
	Right a : es	-> partitionEither' accL       (a : accR) es
	[]		-> (accL, accR)


{-
catLeftOrError :: [Either a b] -> Either [a] b
catLeftOrError    	xx			
	= catLeftOrError' xx []

catLeftOrError' :: 	[Either a b] -> [a] 	-> Either [a] b
catLeftOrError'		[]		aa	=  Left (reverse aa)
catLeftOrError'		(Left  a : xs)	aa	=  catLeftOrError' xs (a : aa)
catLeftOrError'		(Right b : xs)  aa	=  Right b
-}