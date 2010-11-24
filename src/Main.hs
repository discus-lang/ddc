
module Main
	( main
	, ddc )
where
import Main.Setup
import Main.Compile
import Main.Init
import Main.Make
import DDC.Module.Error
import DDC.Module.Scrape
import DDC.Module.ScrapeGraph
import DDC.Main.Pretty
import DDC.Main.Error
import Util
import qualified DDC.Main.ParseArgs	as Arg
import qualified DDC.Config.Version	as Version
import qualified DDC.Main.Arg 		as Arg
import qualified Shared.VarUtil		as Var
import qualified System.Exit		as System
import qualified System.Environment	as System
import qualified System.FilePath	as System
import qualified Data.Map		as Map


-- The Disciplined Disciple Compiler
main :: IO ()
main	
 = do	argStrs	<- System.getArgs
	ddc argStrs


-- | Run the compiler proper.
ddc :: [String] -> IO ()
ddc argStrs
 = do
	-- check args
	let args	= Arg.parse argStrs
	let verbose	= any (== Arg.Verbose) args

	-- print banner if requested
	when verbose
	 $	putStr	$ "* Disciplined Disciple Compiler " ++ Version.version ++ " starting up...\n"

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

		-- Check there are no weird symbols in the name portion of the paths.
		-- We name modules after files, and we don't want bogus symbols
		-- appearing in the module names in later stages.
		| names		<- map System.takeFileName 
				$ compileFiles ++ makeFiles

		, symbols	<- nub  ( filter Var.isSymbol 
					$ concatMap (System.takeFileName)
					$ names) \\ "./"
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
	let impDirs	= map (\p -> if null p then "." else p)
			$ map System.takeDirectory files

	let setup'
		= setup { setupArgsBuild
				=   setupArgsBuild setup
				++ [Arg.ImportDirs impDirs] }

	-- scrape the root modules
	Just roots	<- liftM sequence 
			$  mapM (scrapeSourceFile True) files 

	-- scrape all modules reachable from the roots
	graph		<- scrapeRecursive args setup' roots

	-- during a plain compile, all the dependencies should already be up-to-date
	-- if they're not then complain.
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
   in	compileFile setup' graph mod (shouldBlessMain [mod] mod)
	

