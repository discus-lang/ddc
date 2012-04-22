import DDC.War.Interface.Controller
import DDC.War.Interface.Options
import DDC.War.Interface.Config

import DDC.War.Create
import DDC.War.Driver

import Util.Options
import Util.Options.Help
import BuildBox.Build.BuildState
import BuildBox
import BuildBox.Control.Gang
import BuildBox.Pretty
import BuildBox.IO.Directory
import System.Environment
import System.Directory
import System.IO
import System.Random
import System.Exit
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.STM
import Control.Exception
import Data.List
import Data.Maybe
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
		= filter (not . isInfixOf "skip-")	-- skip over skippable files.
		$ filter (not . isInfixOf "-skip")
		$ filter (not . isInfixOf "war-")	-- don't look at srcs in copied build dirs.
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

	-- Write out a log of failed tests if we were asked to
	when (isJust $ configLogFailed config)
	 $ do   let Just fileLog = configLogFailed config
	        workingDir       <- getCurrentDirectory
{-}
	        let diag jr      = diagnoseJobResults
	                                (configFormatPathWidth config)
	                                False -- no color
	                                workingDir
	                                (jobResultJob jr)
	                                (jobResultResults jr)
	                                
	        let ssResults    = [doc | (success, doc) <- map diag results
	                                , not success ]
-}
                let ssResults = []

	        writeFile fileLog ((render $ vcat ssResults) ++ "\n")
	
	return ()


-- | Run some job chains.
runChains
	:: Config 	-- ^ war configuration
	-> ChanResult	-- ^ channel to write job results to
	-> [Chain]      -- ^ chains of jobs
	-> IO [JobResult]

runChains config chanResult chains
 = do	
	-- Count the total number of chains for the status display.
	let chainsTotal	= length chains
	
	-- Fork a gang to run all the job chains.
	gang	<- forkGangActions (configThreads config)
	 	$ zipWith (runChain config (Just chanResult) chainsTotal)
			[1..]
			chains

	-- Fork the gang controller that manages the console and handles
	-- user input.
	varResults	<- newEmptyMVar
	jobResults      
	 <- forkIO 
	 $ do   results <- controller config gang chainsTotal chanResult
	        putMVar varResults results
	 `finally` (putMVar varResults [])

	-- Wait until the controller to finished
	results <- takeMVar varResults

	-- Wait until the gang is finished running chains, 
	-- or has been killed by the controller.
	joinGang gang

	return results
	


-- | Run a job chain, printing the results to the console.
--   If any job in the chain fails, then skip the rest.
runChain 
	:: Config	       -- ^ war configuration
	-> Maybe ChanResult    -- ^ channel to write job results to
	-> Int	               -- ^ total number of chains
	-> Int		       -- ^ index of this chain
	-> Chain       	       -- ^ chain of jobs to run
	-> IO ()

runChain config mChanResult chainsTotal chainNum (Chain jobs)
 = do	uid		<- getUniqueId
	let state	= (buildStateDefault uid "/tmp")
			{ buildStateLogSystem
				= if configDebug config
					then Just stderr
					else Nothing }
	
        runBuildWithState state
         $  zipWithM (runJob config mChanResult chainNum) 
                [1..] jobs

        return ()

-- | Dispatch a single job of a chain.
runJob
	:: Config 		-- ^ war configuration
	-> Maybe ChanResult	-- ^ channel to write results to
	-> Int			-- ^ index of this chain
	-> Int			-- ^ index of this job of the chain
	-> Job			-- ^ the job to run
	-> Build JobResult

runJob config mChanResult chainNum jobNum job@(Job spec builder)  
 = do	
	-- Run the job.
        result          <- builder

        -- Convert the result into the product the controller wants.
        let product     = productOfResult spec result
	let jobResult   = JobResult chainNum jobNum job product

        case mChanResult of
         Just chanResult
          -> do -- Push the job product into the channel for display.
        	io $ atomically $ writeTChan chanResult jobResult
                return jobResult

         Nothing 
          ->    return jobResult
		

-- | Get a unique(ish) id for this process.
--   The random seeds the global generator with the cpu time in psecs,
--   which should be good enough.
getUniqueId :: IO Integer
getUniqueId
 	= randomRIO (0, 1000000000)	

