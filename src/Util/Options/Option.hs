
module Util.Options.Option
	( Option(..))
where

-- Represents command line options, as well as the command usage help
--	The Option type is parametered by the data type that
--	wraps the command line flags.
data Option a
	-- A single switch that can be enabled, it has no arguments
	--	eg  -O2 
	= OFlag		
		{ optionCtor		:: a
		, optionSwitch		:: [String]	 
		, optionDescription	:: String }

	-- An option that accepts a single string.
	--	eg  -o filename    or    --make  Module.ds
	| OOpt		
		-- Constructor that wraps the string
		{ optionCtorString	:: (String  -> a)	

		-- flags used to invoke option
		--	eg  ["-o", "--output"]
		, optionFlags		:: [String] 		

		-- usage of option
		--	eg "-o <output file>"
		, optionUsage		:: String	

		-- description,
		--	eg "Name of output file"
		, optionDescription	:: String }


	-- An option that accepts several strings.
	--	eg  -c Main.ds Source.ds Thing.ds
	| OOpts	
		{ optionCtorStrings	:: [String] -> a
		, optionFlags		:: [String]
		, optionUsage		:: String
		, optionDescription	:: String }

	-- Build a string of options, without interpreting them
	--	eg  ddc +RTS -O2 --debug   
	--	Loads  "-O2" "--debug" into the OptEscape constructor, 
	--	instead of interpreting them right now.
	--
	| OOptEscape
		{ optionCtorStrings	:: [String] -> a
		, optionFlags		:: [String]
		, optionUsage		:: String
		, optionDescription	:: String }

	| OOptEscapeClose
		{ optionEscapeName	:: String }

	-- Command line args that aren't associated with particular flags
	--	get placed in this default constructor
	| ODefault	
		{ optionCtorString	:: String -> a }


	-- These control how usage help is generated --------------------------
	-- Put a blank line in the usage help
	| OBlank

	-- Name of a page in the usage help
	--	The user can ask for these individually
	| OGroup	
		{ optionPageName	:: String		-- eg "debug"
		, optionPageDescription	:: String }		-- eg "Debugging Flags"

