
-- | Command line option parser.
module Util.Options
	( parseOptions)

where

import Util.Misc
import Util.Pretty
import Util.Data.Either
import Util.Data.List

import Util.Options.Token
import Util.Options.Option

-- | Parse some options from a string
parseOptions :: [Option a] -> [String] -> ([String], [a])
parseOptions options strs
	= munch options $ tokenise (catInt " " strs)


-- | Parse some option strings
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


-- | Select the option which this string refers to
matchOption :: String -> [Option a] -> Maybe (Option a)
matchOption name options
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

