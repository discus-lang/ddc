
module Util.Pretty.Misc
(
	padRc,
	padLc,
	padR,
	padL,
	indent,
	showHex,
	showHexPad,
)

where

import qualified Numeric

-----
padRc	:: Char -> Int -> String -> String
padRc	   c       n      str
	= str ++ take (n - length str) (repeat c)

padR n str	= padRc ' ' n str


padLc	:: Char -> Int -> String -> String
padLc	   c       n      str
	= take (n - length str) (repeat c) ++ str

padL n str	= padLc ' ' n str

-----
indent ::	Int ->	String -> String
indent		n	xx
 = case xx of
 	('\n':xs)	-> "\n" ++ replicate n ' ' ++ indent n xs
	(x:xs)		-> x : indent n xs
	[]		-> []

-----
showHex :: Int -> String
showHex    x 	= Numeric.showHex x ""

showHexPad :: Int -> Int -> String
showHexPad    width  x  =
 let
 	base	= showHex x
 in
 	(take (width - length base) (repeat '0')) ++ base


