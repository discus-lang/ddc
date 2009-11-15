
-- | A graph of scraped module information
--	Used to work out what to build next when doing a 
--	recursive make.

module Module.ScrapeGraph 
	( ScrapeGraph
	, scrapeGraphInsert
	, scrapeRecursive
	, invalidateParents)
where
import Module.Scrape

import Control.Monad		(foldM)
import Main.Arg
import Main.Error
import Main.Setup
import Shared.Var		(Module(..))
import Shared.Pretty
import Shared.Error		(exitWithUserError)

import Util
import Data.Map			(Map)
import Data.List
import Data.Maybe		(catMaybes)
import System.Exit
import qualified Data.Map	as Map

type ScrapeGraph	= Map Module Scrape

-- stage = "Module.ScrapeGraph"

-- | Scrape all modules transtitively imported by these ones.
scrapeRecursive
	:: [Arg]
	-> Setup
	-> [Scrape]			-- root set
	-> IO ScrapeGraph		-- graph of modules reachable from, this one

scrapeRecursive args setup roots
 = let	graph	= Map.fromList [ (scrapeModuleName s, s) | s <- roots]

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
	-> [(Scrape, Module)]
	-> IO ScrapeGraph
scrapeRecursive' args setup graph []
	= return graph
	
scrapeRecursive' args setup graph ((sParent, v):vs)
	-- this module is already in the graph
	| isJust $ Map.lookup v graph
	= scrapeRecursive' args setup graph vs
	
	-- scrape module and add its children
	| otherwise
	= do	mScrapeChild	<- scrapeModule 
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


-- Invalidate parents
--	If some child module needs rebuilding then all its parents do as well.
--	TODO: this'll die if there are cycles in the graph
--	TODO: this is inefficient if the graph is lattice-like instead of a simple tree
--
invalidateParents :: [Arg] -> ScrapeGraph -> Module -> IO ScrapeGraph
invalidateParents args graph mod
 = do	let	Just scrape	= Map.lookup mod graph
	
	-- decend into all the children
	graph'	<- foldM (invalidateParents args) graph (scrapeImported scrape) 

	-- if any of the imports need rebuilding then this one does to
	let rebuild	
		=  (scrapeNeedsRebuild scrape)
		|| (or 	$ map scrapeNeedsRebuild 
			$ map (\m -> let Just sc = Map.lookup m graph' in sc)
			$ scrapeImported scrape)

	let scrape'	= scrape { scrapeNeedsRebuild = rebuild }

	scrapeGraphInsert args mod scrape' graph'


-- A replacement for Map.insert for the ScrapeGraph.
-- This replacement detectd cycles in the import graph as modules are
-- inserted.
--
scrapeGraphInsert :: [Arg] -> Module -> Scrape -> ScrapeGraph -> IO ScrapeGraph
scrapeGraphInsert args m s sg
 = do	case cyclicImport m s sg of
 	 Nothing	-> return $! Map.insert m s sg
	 Just [mc]	-> exitWithUserError args
			 [ ErrorRecursiveModules
			 $ pprStrPlain
                         $ "Module '" % mc % "' imports itself."]
	 Just c		-> exitWithUserError args
			 [ ErrorRecursiveModules
			 $ pprStrPlain
			 $ "Module import graph has cycle : "
			 % punc " -> " (map ppr (head c : reverse c))]

-- Checks to see if adding the Module to the ScrapeGraph would result in a
-- ScrapeGraph with a cycle.
-- If adding the Module will result in a cyclic graph then return the list
-- of modules that constitue a cycle, otherwise return Nothing.
--
cyclicImport :: Module -> Scrape -> ScrapeGraph -> Maybe [Module]
cyclicImport m s sp
 = do	if elem m $ scrapeImported s
	 then Just [m]
	 else listToMaybe
		$ catMaybes
		$ map (\m' -> cyclicImportR m [m] m' sp)
		$ scrapeImported s
     where
	cyclicImportR mx cycle m sp
	 = do	let imports	= concat
				$ map scrapeImported
                                $ catMaybes [Map.lookup m sp]
		if elem mx imports
		 then Just (m : cycle)
		 else listToMaybe
			$ catMaybes
			$ map (\m' -> cyclicImportR mx (m:cycle) m' sp) imports

