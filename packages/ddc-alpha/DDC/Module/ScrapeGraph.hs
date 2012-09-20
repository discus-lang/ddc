{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Module.ScrapeGraph 
	( ScrapeGraph
	, scrapeGraphInsert
	, scrapeRecursive
        , propagateNeedsRebuild)
where
import DDC.Module.Error
import DDC.Module.Scrape
import DDC.Main.Arg
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Main.Setup
import DDC.Var
import Util
import System.Exit
import qualified Data.Map	as Map

-- | A graph of scraped module information
--	Used to determine what to build next when doing a recursive make.
type ScrapeGraph	
	= Map ModuleId Scrape

-- | Scrape all modules transtitively imported by these ones.
scrapeRecursive
	:: [Arg]
	-> Setup
	-> [Scrape]			-- root set
	-> IO ScrapeGraph		-- graph of modules reachable from, this one

scrapeRecursive args setup roots
 = {-# SCC "scrapeRecursive" #-}
   let	graph	= Map.fromList [ (scrapeModuleName s, s) | s <- roots]

	-- each child carries the scrape of the module that imported it.
	--	this is used for error reporting incase we can't find the child
	msImp	= catMap 
			(\s -> [(s, imp)
					| imp <- scrapeImported s])
			roots

   in 	scrapeRecursive' args setup  graph msImp

scrapeRecursive'
	:: [Arg]
	-> Setup
	-> ScrapeGraph
	-> [(Scrape, ModuleId)]
	-> IO ScrapeGraph

scrapeRecursive' _ _ graph []
	= return graph
	
scrapeRecursive' args setup graph ((sParent, v):vs)
	-- this module is already in the graph
	| isJust $ Map.lookup v graph
	= scrapeRecursive' args setup graph vs
	
	-- scrape module and add its children
	| otherwise
	= do	mScrapeChild	<- scrapeSourceModule 
					(importDirsOfSetup setup) 
					True
					v
		case mScrapeChild of
		 Nothing
		  -> do	let Just pathSource	= scrapePathSource sParent
		  	putStr	$ pprStrPlain 
		  		$ "ddc error: can't find source for module '" % v % "'\n"
		  		% "    imported by: " % pathSource % "\n\n"
		  		
			exitFailure
			
		 Just sChild
		  -> do graph'	<- scrapeGraphInsert args v sChild graph
			scrapeRecursive' args setup graph'
				( [(sChild, imp)
					| imp <- scrapeImported sChild]
				  ++ vs)


-- | Invert the ScrapeGraph to create a dependency graph.
dependencyGraph :: ScrapeGraph -> Map ModuleId (Bool, [ModuleId])
dependencyGraph graph
 = {-# SCC "dependencyGraph" #-}
   do	let x = foldl' builder Map.empty
			$ concat
			$ map (\ (k, v) -> map (\i -> (i, needsRebuild' i, k)) (scrapeImported v))
			$ Map.toList graph
	x
	where 	builder m (k, r, v) = case Map.lookup k m of
			Nothing -> Map.insert k (r, [v]) m
			Just _  -> Map.adjust (\(rm, vl) -> (rm || r, v : vl)) k m

		needsRebuild' k = case Map.lookup k graph of
			Nothing -> False
			Just v -> scrapeNeedsRebuild v


-- | This assumes a graph without cycles.
needsRebuild 
	:: Bool 
	-> Map ModuleId (Bool, [ModuleId]) 
	-> [ModuleId] 
	-> ModuleId
	-> [ModuleId]

needsRebuild force graph accum modBuild
 = {-# SCC "needsRebuild" #-}
   do	case (force, Map.lookup modBuild graph) of
 	  (_, Nothing) -> accum
	  (True, Just (_, deps))	-> foldl' (needsRebuild True graph) (deps ++ accum) deps
	  (False, Just (True, deps)) 	-> foldl' (needsRebuild True graph) (deps ++ accum) deps
	  (False, Just (False, _)) 	-> accum


-- | Take a Scrape graph walk the dependencies and set the scrapeNeedsRebuild
--   flag as needed.
propagateNeedsRebuild :: ScrapeGraph -> ScrapeGraph
propagateNeedsRebuild graph
 = {-# SCC "propagateNeedsRebuild" #-}
   let	depGraph	= dependencyGraph graph
	rebuilds	= nub
 			$ foldl' (needsRebuild False depGraph) []
			$ map fst
			$ Map.toList depGraph

   in foldl' (\ g k -> Map.adjust (\v -> v { scrapeNeedsRebuild = True }) k g) graph rebuilds



-- | A replacement for Map.insert for the ScrapeGraph.
--   This replacement detectd cycles in the import graph as modules are
--   inserted.
scrapeGraphInsert
	:: [Arg]
	-> ModuleId
	-> Scrape
	-> ScrapeGraph
	-> IO ScrapeGraph

scrapeGraphInsert args m s sg
 = {-# SCC "scrapeGraphImport" #-}
   case cyclicImport m s sg of

	Nothing
 	  -> return $! Map.insert m s sg

	Just [mc]
	  -> exitWithUserError args
		 [ ErrorRecursiveModules
		 	$ pprStrPlain
                 	$ "Module '" % mc % "' imports itself."]

	Just c
	  -> exitWithUserError args
		[ ErrorRecursiveModules
			$ pprStrPlain
			$ "Module import graph has cycle: "
	 		% punc " -> " (map ppr (head c : reverse c))]


-- | Checks to see if adding the Module to the ScrapeGraph would result in a
--   ScrapeGraph with a cycle.
--   If adding the Module will result in a cyclic graph then return the list
--   of modules that constitue a cycle, otherwise return Nothing.
cyclicImport :: ModuleId -> Scrape -> ScrapeGraph -> Maybe [ModuleId]
cyclicImport m s sp
 = {-# SCC "cyclicImport" #-}
   if elem m $ scrapeImported s
	then Just [m]
	else listToMaybe
		$ catMaybes
		$ map (\m' -> cyclicImportR m [m] m' sp)
		$ scrapeImported s

cyclicImportR mx cycle' m sp
 = do	let imports	= concatMap scrapeImported
                        $ catMaybes [Map.lookup m sp]
	if elem mx imports
	 then Just (m : cycle')
	 else listToMaybe
		$ catMaybes
		$ map (\m' -> cyclicImportR mx (m:cycle') m' sp) imports

