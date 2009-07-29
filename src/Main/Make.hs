
module Main.Make 
	(ddcMake)	
where
import Main.Setup
import Main.Compile
import Main.Link

import Module.Scrape
import Module.ScrapeGraph
import Shared.Pretty
import Shared.Var		(Module)
import qualified Main.Arg as	Arg
import qualified Shared.Var as	Var

import Util
import Util.FilePath
import Util.Graph.Deps
import qualified System.IO	as System
import qualified System.Exit	as System
import qualified System.Directory as System
import qualified Data.Set	as Set
import qualified Data.Map	as Map

-----
-- | Do a recursive make
ddcMake verbose setup files 
 = do
	-- use the directories containing the root files as extra import dirs
	let takeDir path
		= case normalMunchFilePath path of
			(_, dir, _, _)	-> dir

	let setup'	= setup { setupArgsBuild = Arg.ImportDirs (map takeDir files) 
						 : setupArgsBuild setup }
	-- scrape the root modules
	Just roots	
		<- liftM sequence 
		$  mapM (scrapeSourceFile (importDirsOfSetup setup') True) files 
	
	-- scrape all modules reachable from the roots
	graph		<- scrapeRecursive setup' roots

	-- if child modules need rebuilding then parents do
	let graph3	= foldl' invalidateParents graph
			$ map scrapeModuleName roots

	-- dump the scrape graph
--	putStr	$ pprStrPlain
--		$ (punc "\n\n" $ map show $ Map.elems graph') % "\n"
	
	-- count the number of modules needing to be rebuilt
	let buildCount	= length
	 		$ filter scrapeNeedsRebuild 
			$ Map.elems graph3
	
	-- build the required modules
	graph_comp	<- buildLoop setup' graph3 buildCount 0 
			$ map scrapeModuleName roots
	
 	-- If one of the roots is the Main module, then link the binary.
	let gotMain	= any (\r -> scrapeModuleName r == Var.ModuleAbsolute ["Main"])
			$ roots
	
	when (gotMain)
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
	:: Setup		-- ^ compile setup
	-> ScrapeGraph		-- ^ dependency graph
	-> Int			-- ^ total modules needing to be rebuilt
	-> Int			-- ^ ix of this module
	-> [Module]		-- ^ root modules
	-> IO ScrapeGraph

buildLoop setup graph buildCount buildIx roots
 	= buildLoop' 
		setup graph buildCount buildIx roots 
		(findBuildable graph Set.empty roots)

buildLoop' setup graph buildCount buildIx roots build
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
		compileFile setup graph_pruned m

		-- check that the object and interface is actually there
		let (_, fileDir, fileBase, _)
			= normalMunchFilePath pathSource

		let droppedFile ext	= fileDir ++ "/" ++ fileBase ++ ext
		let checkDropped ext 	= do
			let file	= droppedFile ext
			exists		<- System.doesFileExist file
			when (not exists)
			 $ do	putStr $ "ddc error: no " ++ ext ++ " file was produced by compiler."
				System.exitFailure

		checkDropped ".o"
		checkDropped ".di"
		checkDropped ".ddc.h"

		-- update the graph
		let scrape'	= scrape 
					{ scrapeNeedsRebuild 	= False 
					, scrapePathInterface	= Just $ droppedFile ".di"
					, scrapePathHeader	= Just $ droppedFile ".ddc.h"
					, scrapePathObject	= Just $ droppedFile ".o" }
					
		let graph'	= Map.insert m scrape' graph
			
		buildLoop setup graph' buildCount (buildIx + 1) roots



-- Build ------------------------------------------------------------------------------------------
-- | Find a module to build
data Buildable
	= Build Module		-- ok to build this module now
	| Clean			-- there's nothing that needs doing here
	| Blocked		-- there's something potentially buildable, but it's in the blocked set
	deriving (Show, Eq)

isBuild bb	= case bb of { Build _ -> True ; _  -> False }
isClean bb	= case bb of { Clean   -> True ; _  -> False }
isBlocked bb	= case bb of { Blocked -> True ; _  -> False }

-- | Find the next module to build
findBuildable
	:: ScrapeGraph
	-> Set Module
	-> [Module] 
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
	-> Set Module 		-- don't choose one of these modules
	-> Module		-- root of dependency tree
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

