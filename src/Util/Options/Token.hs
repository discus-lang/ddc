
module Util.Options.Token 
	( Token(..)
	, tokenise
	, pprToken)
where

import Util.Test.Check
import Util.Data.List

-- | Tokens for the option parser
data Token
	= TString	String
	| TOption 	String
	deriving (Eq, Show)


-- | Pretty print a token
pprToken :: Token -> String
pprToken (TString str)	= str
pprToken (TOption str)	= "-" ++ str


-- | Tokenize a string containing options
tokenise :: String -> [Token]
tokenise xx
 = case xx of
	[]		-> []
 	('-':xs)	-> tokOption [] xx
	(' ':xs)	-> tokenise  xs
	(x  :xs)	-> tokString [] xx
	
tokOption acc xx
 = case xx of
	[]		-> [TOption acc]
 	(' ':xs)	-> TOption acc : tokenise xs
	(x:xs)		-> tokOption (acc ++ [x]) xs
	
tokString acc xx
 = case xx of
	[]		-> [TString acc]
 	(' ':xs)	-> TString acc : tokenise xs
	(x:xs)		-> tokString (acc ++ [x]) xs

