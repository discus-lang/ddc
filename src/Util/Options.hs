
-- | Command line option parser.
module Util.Options
	( parseOptions)
where
import Util.Misc
import Util.Data.Either
import Util.Data.List
import Util.Options.Token
import Util.Options.Option


-- | Parse some options from a string
parseOptions :: [Option a] -> [String] -> ([String], [a])
parseOptions options strs
	= munch options $ catMap tokenise strs

-- | Parse some option strings
munch 	:: [Option a]  		-- ^ Accepted options
	-> [Token] 		-- ^ Tokens of input
	-> ([String], [a])	-- ^ errors, parsed options

munch	 options	toks
 	= gatherEither $ munchTokens options toks

-- | Convert some tokens to options
munchTokens 
	:: [Option a] 		-- ^ The options we're accepting
	-> [Token] 		-- ^ The tokens still on the input
	-> [Either String a]	-- ^ Left holds error strings, 
				--	Right holds parsed options

munchTokens _       []			= []
munchTokens options toks

	-- An option that we recognise
	| TOption name : ts		<- toks
	, Just option			<- matchOption name options
	= munchTokens_match options option ts

	| TOptionEscape name : ts	<- toks
	, Just option			<- matchOption name options
	= munchTokens_match options option ts

	-- Some string by itself
	--	If we have a default option then use that
	--	Otherwise report an error
	| TString s : ts			<- toks
	= case [ctor | ODefault ctor <- options ] of
		[]	-> [Left $ "unrecognised flag: " ++ s ++ ""]
		[ctor]	-> Right (ctor s) : munchTokens options ts
		_	-> error "Util.Options: multiple default options in list."

	-- An option that isn't in out accepted options list.
	| TOption name : ts		<- toks
	= [Left $ "unrecognised flag: " ++ name ++ ""]

	-- An option that isn't in out accepted options list.
	| TOptionEscape name : ts		<- toks
	= [Left $ "unrecognised flag: " ++ name ++ ""]



munchTokens_match options option ts

	-- A single flag
	| OFlag a names desc	<- option
	= Right a : munchTokens options ts
	
	-- An option that takes a single string.
	| OOpt sf names use desc	<- option
	, TString str : rest		<- ts
	= Right (sf str) : munchTokens options rest

	-- An option that takes several strings
	--	Add all the strings until we reach the next option.
	| OOpts sf names use desc	<- option
	= let	(strToks, rest)	= span (=@= TString{}) ts
		strs		= map (\(TString s) -> s) strToks
	  in	Right (sf strs) : munchTokens options rest

	-- An escape option
	--	Load up all other options until we hit the closing escape,
	--	a new opening esape, or the end of input.
 	| OOptEscape ctor names use desc	<- option
	= let 	(toks,  rest)	= span (\t -> (t =@= TString{} || t =@= TOption{})) ts
		strs		= map stringOfToken toks
	  in	Right (ctor strs) : munchTokens options rest


-- | Select the option which this string refers to
matchOption :: String -> [Option a] -> Maybe (Option a)
matchOption name options
 = case options of
 	[] 
	 -> Nothing

	o@(OFlag a names desc) : os
	 -> if elem name names
	 	then Just o
		else matchOption name os
		
	o@(OOpt a names use desc) : os
	 -> if elem name names
	 	then Just o
		else matchOption name os

	o@(OOpts a names use desc) : os
	 -> if elem name names
	 	then Just o
		else matchOption name os
		
	o@(OOptEscape a names use desc) : os
	 -> if elem name names
		then Just o
		else matchOption name os

	(_ : os)
	 ->	matchOption name os

