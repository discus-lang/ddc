
module Main
	( main
	, ddc )
where

import qualified Main.Arg 		as Arg
import Main.Setup
import Main.Compile
import Main.Version
import Main.Init
import Main.Make

import Module.IO
import Module.Scrape

import Shared.Pretty

import qualified System.IO		as System
import qualified System
import qualified Data.Map		as Map
import Data.Map				(Map)
import Util

-----
main :: IO ()
main	
 = do	argStrings	<- System.getArgs
 	ddc argStrings

ddc :: [String] -> IO ()
ddc argStrings
 = do
	-- check args
	let args	= (Arg.parse $ catInt " " argStrings)
	let verbose	= or $ map (== Arg.Verbose) args

	-- print banner if requested
	when verbose
	 $	putStr	$ "* Disciplined Disciple Compiler " ++ version ++ " starting up...\n"

	-- no args, print help
	when (args == []
	   || (not $ null $ filter (=@= Arg.Help{}) args))
	 $ do	putStr (Arg.helpString args)
		System.exitWith System.ExitSuccess

	-- bad args, bail out
	when (or $ map (=@= Arg.Error{}) args)
	 $ do
		let eArg	= head $ filter (=@= Arg.Error{}) args
		let eString 	= case eArg of { Arg.Error x -> x; }

	 	putStr ("ddc error: bad argument '" ++ eString ++ "'\n")
		System.exitFailure

	-- find the runtime system and base library files
	(pathRuntime, pathLibrary)
			<- verbLocateRunLib verbose args
			
	-- gather up list of files to compile
	let compileFiles	= concat [fs | Arg.Compile fs <- args]
	let makeFiles		= concat [fs | Arg.Make    fs <- args]

	-- make the current setup
	let args'	= args ++ [Arg.OptTailCall, Arg.LintAll]
	let setup
		= Setup
		{ setupArgsCmd 		= args' 
		, setupArgsBuild	= []
		, setupRuntime		= pathRuntime
		, setupLibrary		= pathLibrary 
		, setupRecursive	= Nothing }
		
	let result
		-- don't try to plain compile and make at the same time
		| not $ isNil compileFiles
		, not $ isNil makeFiles
		= do	putStr $ "ddc error: can't specify both -c and -make\n"
			System.exitFailure

		-- do a plain compile
		| not $ isNil compileFiles
		= ddcCompile verbose setup compileFiles
		
		-- do a recursive make
		| not $ isNil makeFiles
		= ddcMake verbose setup makeFiles
		
		-- no input files, bail out
		| otherwise
		= do	putStr ("ddc error: no input files\n")
			System.exitFailure
		
	result

out 	ss	= putStr $ pprStrPlain ss

-- | Do a plain compile	
ddcCompile verbose setup files
 = do 	-- use the directories containing the files to be compiled as extra import dirs
	let takeDir path
		= case normaliseFileName path of
			(_, dir, _, _)	-> dir

	let setup'	
		= setup { setupArgsBuild 
				=   setupArgsBuild setup
				++ [Arg.ImportDirs (map takeDir files)] }

	-- scrape the root modules
	Just roots	
		<- liftM sequence 
		$  mapM (scrapeSourceFile setup') files 

	-- scrape all modules reachable from the roots
	graph		<- scrapeRecursive setup' roots

	-- during a plain compile, all the dependencies should already be up-to-date
	let graph_noRoots 
			= foldr Map.delete graph 
			$ map scrapeModuleName roots
		
	let scrapeDirty
			= filter scrapeNeedsRebuild 
			$ Map.elems graph_noRoots
		
	when (not $ isNil scrapeDirty)
	 $ do 	let dirty1 : _		= scrapeDirty
		let Just fileDirtySrc	= scrapePathSource dirty1
	 	out	$ "ddc error: the '" % scrapeModuleName dirty1 %  "' module needs to be (re)built first.\n"
			% "     when compiling: " % punc " " files 	% "\n"
			% "      module source: " % fileDirtySrc	% "\n\n"

		System.exitFailure

	-- compile input files	
	mapM (compileSingle setup' graph) 
		$ map scrapeModuleName roots

	-- emit a blank line to make things look nicer
	when verbose
	 $ 	putStr "\n"
	
	-- sweet success
	System.exitWith System.ExitSuccess

compileSingle setup graph mod
 = let	Just scrape	= Map.lookup mod graph
 	setup'		= setup { setupArgsBuild
					=  scrapeArgsInline scrape
					++ setupArgsBuild setup }
   in	compileFile setup' graph mod
	

