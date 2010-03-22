import Data.List
import Module.Scrape
import Module.ScrapeGraph
import Shared.Var		(ModuleId(..))
import DDC.Main.Arg
import System.Exit		(exitFailure)

import qualified Data.Map	as Map


genScrape :: [String] -> [[String]] -> (ModuleId, Scrape)
genScrape mod imports
 = (	ModuleId mod,
	Scrape {
		scrapeModuleName 	= ModuleId mod,
		scrapePathSource 	= Nothing,
		scrapePathInterface 	= Nothing,
		scrapePathHeader 	= Nothing,
		scrapePathObject 	= Nothing,
		scrapeImportDir 	= Nothing,
		scrapeImported 		= map ModuleId imports,
		scrapeArgsInline 	= [NoImplicitPrelude],
		scrapeNeedsRebuild 	= False,
		scrapeBuild 		= Nothing,
		scrapeDefinesMain 	= False
		}
	)


invalidateModules :: ScrapeGraph -> [[String]] -> ScrapeGraph
invalidateModules graph names
 = foldl' (\ g m ->
 	Map.adjust (\s -> s { scrapeNeedsRebuild = True }) m g
 	) graph $ map ModuleId names


needsRebuild :: ScrapeGraph -> [ModuleId]
needsRebuild graph
 =	foldl' (\ l (m, v) -> if scrapeNeedsRebuild v then (m : l) else l) [] $ Map.toList graph


defaultScrapeGraph :: ScrapeGraph
defaultScrapeGraph
 = Map.fromList $ map (\ (m, i) -> genScrape m i)
 	[ (["A"], [["B"], ["C"]])
	, (["B"], [["C"]])
	, (["C"], [])
	, (["D", "E"], [["D", "F"]])
	, (["D", "F"], [["D", "G"]])
	, (["D", "G"], [["D", "H"]])
	, (["D", "H"], [["D", "I"]])
	, (["D", "I"], [])
	, (["Main"], [["A"],["B"], ["D", "E"]]) ]


runTest :: Int -> [[String]] -> IO ()
runTest expected mods
 = do	let graph2 	= invalidateModules defaultScrapeGraph mods
	let graph3 	= propagateNeedsRebuild graph2
	let rmods 	= needsRebuild graph3
	let count 	= length rmods
	if count == expected
	 then return ()
         else do
		putStrLn $ "Test for '" ++ show mods ++ "' failed. Expected " ++ show expected ++ " got " ++ show count ++ "."
		putStrLn $ "Rebuilding : " ++ show rmods
		exitFailure


main :: IO ()
main
 = do
	runTest 1 [["Main"]]
	runTest 2 [["A"]]
	runTest 3 [["Main"], ["B"]]
	runTest 4 [["C"]]
	runTest 3 [["D", "F"]]
	runTest 4 [["D", "G"]]
	runTest 5 [["D", "H"]]
	putStrLn "Pass.\n"

