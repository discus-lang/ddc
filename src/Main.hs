
module Main
	( main
	, ddc )
where
import Main.Setup
import Main.Compile
import Main.Version
import Main.Init
import Main.Make
import Main.Error
import qualified Main.Arg 		as Arg

import Module.Scrape
import Module.ScrapeGraph
import Shared.Pretty
import Shared.Error
import qualified Shared.VarUtil		as Var

import Util
import Util.FilePath
import Util.Test.Check
import qualified System.IO		as System
import qualified System
import qualified Data.Map		as Map


-- The Disciplined Disciple Compiler
main :: IO ()
main	
 = do	argStrs	<- System.getArgs

	-- If we have a single --test option, then override everything else
	--	and run all our QuickCheck tests.
 	case argStrs of 
	 ["--test"]	-> runTests
	 _		-> ddc argStrs


-- | Run all our QuickCheck tests.
runTests :: IO ()
 =	checkTests test_Util


-- | Run the compiler proper.
ddc :: [String] -> IO ()
ddc argStrs
 = do
	-- check args
	let args	= Arg.parse argStrs
	let verbose	= any (== Arg.Verbose) args

	-- print banner if requested
	when verbose
	 $	putStr	$ "* Disciplined Disciple Compiler " ++ version ++ " starting up...\n"

	-- no args, print help
	when (args == []
	   || not (null $ filter (=@= Arg.Help{}) args))
	 $ do	putStr (Arg.helpString args)
		System.exitWith System.ExitSuccess

	-- bad args, bail out
	when (any (=@= Arg.Error{}) args)
	 $ do
		let eArg	= head $ filter (=@= Arg.Error{}) args
		let eString 	= case eArg of { Arg.Error x -> x; }

	 	putStr ("ddc error: " ++ eString ++ "\n")
		System.exitFailure

	-- find the runtime system and base library files
	(pathRuntime, pathLibrary)
			<- verbLocateRunLib verbose args

	-- gather up list of files to compile, build or make
	let compileFiles	= concat [files | Arg.Compile   files	<- args]

	let buildFiles		= concat [files | Arg.Build     files	<- args]
	
	let makeFiles		= concat [files | Arg.Make 	files   <- args]
				++ 	 [files | Arg.InputFile files   <- args]
	
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
		| symbols	<- nub  ( filter Var.isSymbol 
					$ concatMap fileNameOfPath
					$ compileFiles ++ makeFiles) \\ "./"
		, Just sym	<- takeHead symbols
		= exitWithUserError args [ErrorSymbolInFileName sym]

		-- don't try to plain compile and make at the same time
		| not $ isNil compileFiles
		, not $ isNil makeFiles
		= do	putStr "ddc error: can't specify both -c and -make\n"
			System.exitFailure

		-- do a plain compile
		| not $ isNil compileFiles
		= ddcCompile args verbose setup compileFiles

		-- do a recursive build
		| not $ isNil buildFiles
		= ddcMake args verbose setup False buildFiles
		
		-- do a recursive build, then link executable
		| not $ isNil makeFiles	
		= ddcMake args verbose setup True makeFiles
		
		-- no input files, bail out
		| otherwise
		= do	putStr "ddc error: no input files\n"
			System.exitFailure
		
	result


-- | Do a regular compile.
ddcCompile args verbose setup files
 = do 	-- use the directories containing the files to be compiled as extra import dirs
	let takeDir path
		= case normalMunchFilePath path of
			(_, dir, _, _)	-> dir

	let setup'	
		= setup { setupArgsBuild 
				=   setupArgsBuild setup
				++ [Arg.ImportDirs (map takeDir files)] }

	-- scrape the root modules
	Just roots	
		<- liftM sequence 
		$  mapM (scrapeSourceFile (importDirsOfSetup setup') True) files 

	-- scrape all modules reachable from the roots
	graph		<- scrapeRecursive args setup' roots

	-- during a plain compile, all the dependencies should already be up-to-date
	let graph_noRoots 
			= foldr Map.delete graph 
			$ map scrapeModuleName roots
		
	let scrapeDirty
			= filter scrapeNeedsRebuild 
			$ Map.elems graph_noRoots
		
	unless (isNil scrapeDirty)
	 $ do 	let dirty1 : _		= scrapeDirty
		let Just fileDirtySrc	= scrapePathSource dirty1
	 	putStr 	$ pprStrPlain
			$ "ddc error: the '" % scrapeModuleName dirty1 %  "' module needs to be (re)built first.\n"
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
   in	compileFile setup' graph mod False
	

