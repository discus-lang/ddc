
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


-- Data to hold an option parser
--	parametered by the data type that will represent them
data Option a
	= ODefault	([String] -> a)
	| OGroup	String String
	| OFlag		a		[String] String
	| OOpt		( String  -> a)	[String] String String
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
		
	(o@(OOpt a names use desc) : os)
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
	
	| OOpt sf names use desc	<- option
	, TString str : rest		<- ts
	= Right (sf str) : munchR options rest

	| OOpts sf names use desc	<- option
	= let	(strToks, rest)	= span (=@= TString{}) ts
		strs		= map (\(TString s) -> s) strToks
	  in	Right (sf strs) : munchR options rest


-- | Make a help page from this list of options
makeOptionHelp 
	:: Int 			-- indent level of descriptions
	-> [String] 		-- tags of the sections to show
	-> [Option a] 		-- options
	-> String

makeOptionHelp indent secs os
	= concat 
	$ (if elem "contents" secs
		then "\n  -- Contents --\n"
		else "")
	:  makeOptionHelp' indent secs False os

makeOptionHelp' _ _ _ []	
	= []
	
makeOptionHelp' indent secs squash (o:os)
 = case o of
	OGroup tag name
	 -- print a contents entry, but not the body of the section
	 | elem "contents" secs
	 -> "  " : makeHelp indent o : makeOptionHelp' indent secs True os

	 -- print an interesting section
	 | elem tag secs || elem "all" secs
	 -> "\n  " : makeHelp indent o : makeOptionHelp' indent secs False os
		
	 -- skip over a boring section
	 | otherwise
	 -> makeOptionHelp' indent secs True os
		
	_ 
		| squash	
		-> makeOptionHelp' indent secs squash os
		
		| otherwise	
		-> makeHelp indent o
		:  makeOptionHelp' indent secs squash os
			

makeHelp indent o
 = case o of
	ODefault optF
	 -> ""

 	OGroup tag name	
	 -> name ++ " (" ++ tag ++ ")\n"

	OFlag  opt names desc
	 -> pprStr ()
	 $  padL indent ("    " % punc ", " names) % desc % "\n"

	OOpt  optF names use desc
	 -> pprStr ()
	 $  padL indent ("    " % use) % desc % "\n"

	OOpts  optF names use desc
	 -> pprStr ()
	 $  padL indent ("    " % use) % desc % "\n"
	 
	OBlank
	 -> "\n"



