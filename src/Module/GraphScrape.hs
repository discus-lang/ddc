
module Module.GraphScrape 
	( GraphScrape
	, invalidateParents)
where

import Module.Scrape

import Shared.Var		(Module)

import qualified Data.Map	as Map
import Data.Map			(Map)
import Data.List


type GraphScrape	= Map Module Scrape

-- Invalidate parents
--	If some child module needs rebuilding then all its parents do as well.
--	TODO: this'll die if there are cycles in the graph
--	TODO: this is inefficient if the graph is lattice like instead of a simple tree
invalidateParents :: GraphScrape -> Module -> GraphScrape
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
