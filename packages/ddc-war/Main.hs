
import DDC.War.Interface.Controller
import DDC.War.Interface.Option
import DDC.War.Interface.Config

import DDC.War.Create
import DDC.War.Driver
import DDC.War.Driver.Gang

import BuildBox.Control.Gang
import BuildBox.IO.Directory
import System.Environment
import System.Directory
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.STM
import Control.Exception
import Data.List
import qualified Data.Sequence		as Seq
import qualified Data.Foldable		as Seq
import qualified Data.Set		as Set
import qualified Data.Traversable	as Seq


main :: IO ()
main 
 = do	-- Parse command line options, and exit if they're no good.
	args           <- getArgs
	let config      = parseOptions args defaultConfig
		
	-- All the starting test directories from the command line.
	testDirs       <- mapM (makeRelativeToCurrentDirectory <=< canonicalizePath)
                        $  configTestDirs config

	-- Trace all the files reachable from these directories.
	testFilesRaw   <- liftM (join . Seq.fromList)
                        $  mapM traceFilesFrom testDirs
		
	-- Canonicalize all the paths and put them in a set (which sorts them)
	testFilesSet   <- liftM (Set.fromList . Seq.toList)
                        $  Seq.mapM canonicalizePath
                        $  testFilesRaw

        -- Skip over files with 'skip' in the path,
        --  and don't decend into our own build dirs.
	let testFilesSorted
		= filter (not . isInfixOf "skip-")
		$ filter (not . isInfixOf "-skip")
		$ filter (not . isInfixOf "war-")
		$ Set.toList testFilesSet

        let testFilesSortedSet
                = Set.fromList testFilesSorted

	-- Create test chains based on the files we have.
	let ways'
		= case configWays config of
		   []	-> [Way "std" [] []]
		   ways	-> ways

	let chains :: [Chain]
	    chains = concat 
                [ concat $ map (\way -> create way testFilesSortedSet file) ways'
		| file <- testFilesSorted]

	-- Channel for threads to write their results to.
	(chanResult :: ChanResult)
		<- atomically $ newTChan

	-- Run all the chains.
	results <- runChains config chanResult chains
	
	return ()


-- | Run some job chains.
runChains
	:: Config 	-- ^ war configuration
	-> ChanResult	-- ^ channel to write job results to
	-> [Chain]      -- ^ chains of jobs
	-> IO [Result]

runChains config chanResult chains
 = do	
	-- Count the total number of chains for the status display.
	let chainsTotal	= length chains
	
        -- Fork a gang to run all the job chains.
        gang    <- forkChainsIO 
                        (configThreads config) ("/tmp")
                        (Just chanResult) chains

        -- Fork the gang controller that manages the console and handles
        -- user input.
        varResults      <- newEmptyMVar
        jobResults      
         <- forkIO 
         $ do   results <- controller config gang chainsTotal chanResult
                putMVar varResults results
         `finally` (putMVar varResults [])


	-- Wait for the controller to finish.
	results <- takeMVar varResults

	-- Wait until the gang is finished running chains, 
	-- or has been killed by the controller.
	joinGang gang

	return results
	


