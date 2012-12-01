
module Util.Pretty.Misc
	( indentSpace
	, showHex
	, showHexPad)

where

import qualified Numeric

indentSpace :: Int ->	String -> String
indentSpace n xx
 = case xx of
 	('\n':xs)	-> "\n" ++ replicate n ' ' ++ indentSpace n xs
	(x:xs)		-> x : indentSpace n xs
	[]		-> []

showHex :: Int -> String
showHex    x 	= Numeric.showHex x ""

showHexPad :: Int -> Int -> String
showHexPad    width  x  =
 let 	base	= showHex x
 in 	replicate (width - length base) '0' ++ base


