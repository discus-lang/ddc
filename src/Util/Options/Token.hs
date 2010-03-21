
module Util.Options.Token 
	( Token(..)
	, tokenise
	, pprToken
	, stringOfToken)
where


-- | Tokens for the option parser
data Token
	= TString	String
	| TOption 	String
	| TOptionEscape	String
	deriving (Eq, Show)


-- | Pretty print a token
pprToken :: Token -> String
pprToken tt 
 = case tt of
	TString str		-> str
	TOption str		-> "-" ++ str
	TOptionEscape str	-> "+" ++ str

stringOfToken :: Token -> String
stringOfToken tt
 = case tt of
	TString str		-> str
	TOption str		-> str
	TOptionEscape str	-> str

-- | Tokenize a string containing options
tokenise :: String -> [Token]
tokenise xx
 = case xx of
	[]		-> []
 	('-':xs)	-> tokOption 	   [] xx
	('+':xs)	-> tokOptionEscape [] xx
	(' ':xs)	-> tokenise  xs
	(x  :xs)	-> tokString [] xx
	
tokOption acc xx
 = case xx of
	[]		-> [TOption acc]
 	(' ':xs)	-> TOption acc : tokenise xs
	(x:xs)		-> tokOption (acc ++ [x]) xs

tokOptionEscape acc xx
 = case xx of
	[]		-> [TOptionEscape acc]
 	(' ':xs)	-> TOptionEscape acc : tokenise xs
	(x:xs)		-> tokOptionEscape (acc ++ [x]) xs

tokString acc xx
 = case xx of
	[]		-> [TString acc]
 	(' ':xs)	-> TString acc : tokenise xs
	(x:xs)		-> tokString (acc ++ [x]) xs

