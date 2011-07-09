
import DDC.War.Options
import DDC.War.Way	()
import DDC.War.Config
import DDC.War.Job
import DDC.War.JobCreate
import DDC.War.JobDispatch
import DDC.War.Gang
import DDC.War.Pretty
import Util.Options
import Util.Options.Help
import BuildBox
import System.Environment
import System.Directory
import Control.Monad
import Data.List
import qualified Data.Sequence		as Seq
import qualified Data.Foldable		as Seq
import qualified Data.Set		as Set
import qualified Data.Traversable	as Seq


main :: IO ()
main 
 = do	-- Parse command line options, and exit if they're no good.
	args	<- getArgs
	let (errs, options)	= parseOptions warOptions args
	let help		= makeOptionHelp 30 ["all"] warOptions 

	-- Print command usage if asked for.
	when (elem OptHelp options)
	 $ do	putStrLn $ help
		exitSuccess

	-- Print errors if there are any.
	when (not $ null errs)
	 $ do	putStrLn $ (concat $ intersperse "\n" errs) 
		putStrLn $ help
		exitFailure

	-- Load war config from the cmd line options
	let config = loadConfig options
		
	-- All the starting test directories from the command line.
	testDirs
		<- mapM (makeRelativeToCurrentDirectory <=< canonicalizePath)
		$  [dirs | OptTestDir dirs <- configOptions config]

	-- Trace all the files reachable from these directories.
	testFilesRaw
		<- liftM (join . Seq.fromList)
		$  mapM traceFilesFrom testDirs
		
	-- Canonicalize all the paths and put them in a set (which sorts them)
	testFilesSet
		<- liftM (Set.fromList . Seq.toList)
		$  Seq.mapM canonicalizePath
		$  testFilesRaw

	let testFilesSorted
		= Set.toList testFilesSet

	-- Create test jobs based on the files we have.
	let jobChains :: [[Job]]
	    jobChains
		= filter (not . null)
		$ map    (createJobs "normal" testFilesSet)
		$ filter (not . isInfixOf "skip-")	-- skip over skippable files.
		$ filter (not . isInfixOf "-skip")
		$ filter (not . isInfixOf "war-")	-- don't look at srcs in copied build dirs.
		$ testFilesSorted

	runJobChains config jobChains

	return ()


-- | Run some job chains.
runJobChains :: Config -> [[Job]] -> IO ()
runJobChains config jcs
 = do	let chainsTotal	= length jcs

	runParActions 4
		$ map (\chain ix -> runJobChain config chainsTotal ix chain) jcs

	return ()

-- | Run a job chain, printing the results to the console.
--   If any job in the chain fails, then skip the rest.
runJobChain :: Config -> Int -> Int -> [Job] -> IO ()
runJobChain config chainsTotal chainNum chain
 = do	runBuild ("/tmp/war" ++ show chainNum) 
 		$ zipWithM_ (dispatch config chainsTotal chainNum) [0..] chain

	return ()

dispatch :: Config -> Int -> Int -> Int -> Job -> Build ()
dispatch config chainsTotal chainNum jobNum job
 = do	results		<- dispatchJob job
	dirWorking	<- io $ getCurrentDirectory
	let useColor	= not $ configBatch config
	let width	= configFormatPathWidth config

	outLn 	$  parens (padR (length $ show chainsTotal)
				(ppr $ chainNum + 1) 
				<> text "."
				<> ppr (jobNum + 1)
				<> text "/" 
				<> ppr chainsTotal)
		<> space
		<> pprJobResult width useColor dirWorking job results

	return ()
