
module Util.Either 
(
	gatherEither,
	catLeftOrError
)

where

import Data.Either


gatherEither ::		[Either a b]		-> ([a], [b])
gatherEither		ee
 = let	(accL, accR)	= gatherEither' [] [] ee
   in	(reverse accL, reverse accR)

gatherEither'	accL accR ee
 = case ee of
 	Left  a : es	-> gatherEither' (a : accL) accR 	es
	Right a : es	-> gatherEither' accL       (a : accR)	es
	[]		-> (accL, accR)


catLeftOrError :: 	[Either a b] 		-> Either [a] b
catLeftOrError    	xx			
	= catLeftOrError' xx []

catLeftOrError' :: 	[Either a b] -> [a] 	-> Either [a] b
catLeftOrError'		[]		aa	=  Left (reverse aa)
catLeftOrError'		(Left  a : xs)	aa	=  catLeftOrError' xs (a : aa)
catLeftOrError'		(Right b : xs)  aa	=  Right b

