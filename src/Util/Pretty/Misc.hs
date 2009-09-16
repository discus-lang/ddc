
module Util.Pretty.Misc
	( indent
	, showHex
	, showHexPad)

where

import qualified Numeric

indent ::	Int ->	String -> String
indent		n	xx
 = case xx of
 	('\n':xs)	-> "\n" ++ replicate n ' ' ++ indent n xs
	(x:xs)		-> x : indent n xs
	[]		-> []

showHex :: Int -> String
showHex    x 	= Numeric.showHex x ""

showHexPad :: Int -> Int -> String
showHexPad    width  x  =
 let 	base	= showHex x
 in 	replicate (width - length base) '0' ++ base


