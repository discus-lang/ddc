
-- | A graph of scraped module information
--	Used to work out what to build next when doing a 
--	recursive make.

module Module.ScrapeGraph 
	( ScrapeGraph
	, scrapeRecursive
	, invalidateParents)
where
import Module.Scrape

import Main.Setup
import Shared.Var		(Module)
import Shared.Pretty

import Util
import Data.Map			(Map)
import Data.List
import System.Exit
import qualified Data.Map	as Map

type ScrapeGraph	= Map Module Scrape

-- | Scrape all modules transtitively imported by these ones.
scrapeRecursive
	:: Setup
	-> [Scrape]			-- root set
	-> IO (Map Module Scrape)	-- graph of modules reachable from, this one

scrapeRecursive setup roots
 = let	graph	= Map.fromList [ (scrapeModuleName s, s) | s <- roots]

	-- each child carries the scrape of the module that imported it.
	--	this is used for error reporting incase we can't find the child
	msImp	= catMap 
			(\s -> [(s, imp)
					| imp <- scrapeImported s])
			roots

   in 	scrapeRecursive' setup  graph msImp

scrapeRecursive' setup graph []
	= return graph
	
scrapeRecursive' setup graph ((sParent, v):vs)
	-- this module is already in the graph
	| isJust $ Map.lookup v graph
	= scrapeRecursive' setup graph vs
	
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
		  ->	scrapeRecursive' 
				setup 
				(Map.insert v sChild graph)
				( [(sChild, imp)
					| imp <- scrapeImported sChild]
				  ++ vs)


-- Invalidate parents
--	If some child module needs rebuilding then all its parents do as well.
--	TODO: this'll die if there are cycles in the graph
--	TODO: this is inefficient if the graph is lattice-like instead of a simple tree
--
invalidateParents :: ScrapeGraph -> Module -> ScrapeGraph
invalidateParents graph mod
 = let	Just scrape	= Map.lookup mod graph
	
	-- decend into all the children
	graph'	= foldl' invalidateParents graph (scrapeImported scrape) 

	-- if any of the imports need rebuilding then this one does to
	rebuild	
		=  (scrapeNeedsRebuild scrape)
		|| (or 	$ map scrapeNeedsRebuild 
			$ map (\m -> let Just sc = Map.lookup m graph' in sc)
			$ scrapeImported scrape)

	scrape'	= scrape { scrapeNeedsRebuild = rebuild }
	
   in	Map.insert mod scrape' graph'

