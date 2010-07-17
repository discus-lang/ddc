
module Main.Make 
	( ddcMake
	, shouldBlessMain )	
where
import Main.Setup
import Main.Compile
import Main.Link
import Main.Error
import Module.Scrape
import Module.ScrapeGraph
import Util
import Util.FilePath
import Util.Graph.Deps
import DDC.Var
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Main.Arg			(Arg)
import qualified DDC.Main.Arg 		as Arg
import qualified System.IO		as System
import qualified System.Exit		as System
import qualified System.Directory 	as System
import qualified Data.Set		as Set

-- Map.insert is hidden. Use scrapeGraphInsert instead.
import qualified Data.Map	as Map hiding (insert)

-----
-- | Do a recursive make
ddcMake args verbose setup linkExecutable files 
 = do
	-- use the directories containing the root files as extra import dirs
	let takeDir path
		= case normalMunchFilePath path of
			(_, dir, _, _)	-> dir

	let setup'	= setup { setupArgsBuild = Arg.ImportDirs (map takeDir files) 
						 : setupArgsBuild setup }
	-- Scrape the root modules.
	Just roots_
		<- liftM sequence 
		$  mapM (scrapeSourceFile (importDirsOfSetup setup') True) files 
	
	-- Force the roots to be rebuilt. 
	--	This ensures that we'll see the main function if it's sensibly defined in the program.
	let roots	
		| linkExecutable = map (\s -> s { scrapeNeedsRebuild = True }) roots_
		| otherwise	= roots_
	
	-- scrape all modules reachable from the roots
	graph		<- scrapeRecursive args setup' roots

	-- if child modules need rebuilding then parents do
	let graph3 = propagateNeedsRebuild graph

	-- count the number of modules needing to be rebuilt
	let buildCount	= length
	 		$ filter scrapeNeedsRebuild 
			$ Map.elems graph3
	
	-- build the required modules
	graph_comp	<- buildLoop args setup' graph3 buildCount 0 
			$ map scrapeModuleName roots
	
 	-- Check if one of the modules defines the main function
	let gotMain	= any (\r -> scrapeDefinesMain r)
			$ Map.elems graph_comp
	
	when (linkExecutable && not gotMain)
	 $ exitWithUserError args [ErrorLinkExecutableWithoutMain]
	
	when (linkExecutable && gotMain)
	 $ do	-- all the object files that need to be linked
		let Just objFiles	
			= sequence 
			$ map scrapePathObject 
			$ Map.elems graph_comp

		let root1 : _	= roots
		linkFile setup (scrapeBuild root1) objFiles
	
	
	-- sweet success
	System.exitWith System.ExitSuccess

buildLoop 
	:: [Arg]
	-> Setup		-- ^ compile setup
	-> ScrapeGraph		-- ^ dependency graph
	-> Int			-- ^ total number of modules needing to be rebuilt
	-> Int			-- ^ ix of this module
	-> [ModuleId]		-- ^ root modules
	-> IO ScrapeGraph

buildLoop args setup graph buildCount buildIx roots
 	= buildLoop' 
		args setup graph buildCount buildIx roots 
		(findBuildable graph Set.empty roots)

buildLoop' args setup graph buildCount buildIx roots build
	-- all done
	| Clean		<- build
	= do	return graph
	
	-- build one file 
	| Build m	<- build
	= do	
		-- print what module we're compiling
		let pad	= length $ show buildCount

		when (not $ elem Arg.Quiet $ setupArgs setup)
		 $ do	putStr 	
			 	$ pprStrPlain
				$ "[" 
				% padR pad (show $ buildIx + 1) 
				% " of "
				% padR pad (show buildCount)
				% "]"
				% " Compiling " 
				% m 
				% "\n"

			System.hFlush System.stdout

		-- chop down the scrape graph to just the modules needed by
		--	the one being compiled. compileFile uses this to work out
		--	what sea .h files to include in the generated source.
		let graph_modImports	
			= Map.fromList
			$ [ (scrapeModuleName s, Set.fromList $ scrapeImported s)
				| s <- Map.elems graph]
							
		let graph_modImports_pruned
			= graphPrune graph_modImports m
			
		let graph_pruned
			= Map.filter (\s -> Map.member (scrapeModuleName s) graph_modImports_pruned) graph
			
		-- run the compiler to produce the object file
		let Just scrape		= Map.lookup m graph_pruned
		let Just pathSource	= scrapePathSource scrape
		definesMain		<- compileFile setup graph_pruned m (shouldBlessMain roots m) 

		-- check that the object and interface is actually there
		let (_, fileDir, fileBase, _)
			= normalMunchFilePath pathSource

		let droppedFile ext	= fileDir ++ "/" ++ fileBase ++ ext
		let checkDropped ext 	= do
			let file	= droppedFile ext
			exists		<- System.doesFileExist file
			when (not exists)
			 $ do	putStrLn $ "ddc error: no " ++ ext ++ " file was produced by compiler."
				System.exitFailure

		checkDropped ".o"
		checkDropped ".di"
		checkDropped ".ddc.h"

		-- update the graph
		let scrape'	= scrape 
					{ scrapeNeedsRebuild 	= False 
					, scrapePathInterface	= Just $ droppedFile ".di"
					, scrapePathHeader	= Just $ droppedFile ".ddc.h"
					, scrapePathObject	= Just $ droppedFile ".o" 
					, scrapeDefinesMain	= definesMain }
					
		graph'		<- scrapeGraphInsert args m scrape' graph
			
		buildLoop args setup graph' buildCount (buildIx + 1) roots



-- Build ------------------------------------------------------------------------------------------
-- | Find a module to build
data Buildable
	= Build ModuleId	-- ok to build this module now
	| Clean			-- there's nothing that needs doing here
	| Blocked		-- there's something potentially buildable, but it's in the blocked set
	deriving (Show, Eq)

isBuild bb	= case bb of { Build _ -> True ; _  -> False }
isClean bb	= case bb of { Clean   -> True ; _  -> False }
isBlocked bb	= case bb of { Blocked -> True ; _  -> False }

-- | Find the next module to build
findBuildable
	:: ScrapeGraph
	-> Set ModuleId
	-> [ModuleId] 
	-> Buildable

findBuildable graph noChoose roots
	-- all clean
	| and $ map isClean builds
	= Clean

	-- something is buildable
	| b@(Build _) : _	<- filter isBuild builds
	= b
		
	-- something is bloccked
	| or $ map isBlocked builds
	= Blocked

	where	builds	= map (findBuildable1 graph noChoose) roots
		

findBuildable1 
	:: ScrapeGraph		-- module scrape graph
	-> Set ModuleId 	-- don't choose one of these modules
	-> ModuleId		-- root of dependency tree
	-> Buildable

findBuildable1 graph noChoose mod
 = let	Just scrape	= Map.lookup mod graph

	-- see if any of the children are buildable
	childBuild	= map (findBuildable1 graph noChoose) 
			$ scrapeImported scrape
	
	result
		-- if everyone's clean then everyone's happy
		| and $ map isClean childBuild
		, not $ scrapeNeedsRebuild scrape
		= Clean

		-- if all the children are clean, and this module needs rebuilding 
		--	then use that, so long is its not blocked
		| and $ map isClean childBuild
		, scrapeNeedsRebuild scrape
		= if Set.member mod noChoose
			then Blocked
			else Build (scrapeModuleName scrape)

		-- if one of the children is buildable then use that
		| b@(Build _) : _	<- filter isBuild childBuild
		= b
		
		-- otherwise something must be blocked
		| or $ map isBlocked childBuild
		= Blocked
			
   in	result


-- | Decide whether to treat a 'main' function defined in this module
--	as the program entry point.
shouldBlessMain :: [ModuleId] -> ModuleId -> Bool
shouldBlessMain roots m

	-- We have Main as a root, but we're not compiling it now.
	| elem mainModule roots
	, m /= mainModule
	= False

	-- We have Main as a root, and we're compiling it now
	| elem mainModule roots
	, m == mainModule
	= True
	
	-- If we don't have Main as a root, then treat all root modules as Main.
	| otherwise
	= True

	where	mainModule = ModuleId ["Main"]
