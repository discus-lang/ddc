
module Util.Options
(
	Option(..),
	matchOption,
	
	Token(..),
	tokenise,

	munch,
	makeOptionHelp
)

where

import Util.Misc
import Util.Either
import Util.List
import Util.Pretty

-----------------------
-- Option
--
data Option a
	= ODefault	([String] -> a)
	| OGroup	String String
	| OFlag		a		[String] String
	| OOpts		([String] -> a)	[String] String String
	| OBlank

matchOption ::	String ->	[Option a]	-> Maybe (Option a)
matchOption	name		options
 = case options of
 	[] 
	 -> Nothing

	(o@(OFlag a names desc) : os)
	 -> if elem name names
	 	then Just o
		else matchOption name os
		
	(o@(OOpts a names use desc) : os)
	 -> if elem name names
	 	then Just o
		else matchOption name os
		
	(_ : os)
	 ->	matchOption name os


-----------------------
-- Token
-- 
data Token
	= TString	String
	| TOption 	String
	deriving Show
	
tokenise ::	String -> [Token]
tokenise	xx
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
	
-------------------------
-- munch
--
munch :: [Option a]  -> [Token] -> ([String], [a])
munch	 options	toks
 	= gatherEither $ munchS options toks


munchS :: [Option a] -> [Token] -> [Either String a]
munchS	  options	toks
 = case options of
 	ODefault sf : _
	 -> let	(strToks, rest)	= span (=@= TString{}) toks
		strs		= map (\(TString s) -> s) strToks
	    in  case strs of
	    	 []	-> munchR options toks
		 _	-> Right (sf strs) : munchR options rest
	    
	_ -> munchR options toks


munchR :: [Option a] ->	[Token] -> [Either String a]
munchR	 options	toks

	| []				<- toks
	= []

	| t@(TOption name) : ts		<- toks
	, Just option			<- matchOption name options
	= munchOption options option ts

	| TOption name : ts		<- toks
	= [Left $ "Unknown option: '" ++ name ++ "'"]
	
	| TString s : _			<- toks
	= [Left $ "Unexpected non-option: '" ++ s ++ "'"]
	
		
munchOption options option ts

	| OFlag a names desc	<- option
	= Right a : munchR options ts
	
	| OOpts sf names use desc	<- option
	= let	(strToks, rest)	= span (=@= TString{}) ts
		strs		= map (\(TString s) -> s) strToks
	  in	Right (sf strs) : munchR options rest


makeOptionHelp :: Int -> [Option a] -> String
makeOptionHelp descIndent x
	= concat 
	$ map (makeOptionHelp' descIndent)
	$ x


makeOptionHelp' descIndent o
 = case o of
	ODefault optF
	 -> ""

 	OGroup tag name	
	 -> "\n  " ++ name ++ "\n"

	OFlag  opt names desc
	 -> padR descIndent ("    " ++ (catInt ", " $ names))
	 ++ desc ++ "\n"

	OOpts  optF names use desc
	 -> padR descIndent ("    " ++ use)
	 ++ desc ++ "\n"
	 
	OBlank
	 -> "\n"



