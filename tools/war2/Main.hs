
import Config
import Test
import Command
import War
import Timing
import Format
import Dispatch

import Util
import Util.Options
import Util.Terminal.VT100

import System.Environment
import System.Exit
import System.Time
import System.IO
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.MVar

import Dispatch.Worker

import qualified Dispatch.BackGraph	as BackGraph
import Dispatch.BackGraph		(BackNode(..))

import qualified Dispatch.WorkGraph	as WorkGraph
import Dispatch.WorkGraph		(WorkGraph, WorkNode(..))

import qualified Data.Map		as Map
import Data.Map				(Map)
	
import qualified Data.Set		as Set
import Data.Set				(Set)

-- Main -------------------------------------------------------------------------------------------
main :: IO ()
main 
 = do	-- Parse command line options, and exit if they're no good.
	args	<- getArgs
	let (errs, options)	= parseOptions args
	let help		= makeOptionHelp 30 ["all"] warOptions 

	when (elem OptHelp options)
	 $ do	putStr $ help ++ "\n"
		exitSuccess

	when (not $ null errs)
	 $ do	putStr $ (catInt "\n" errs) ++ "\n"
		putStr $ help ++ "\n"
		exitFailure

	-- Setup config
	let config
		= Config
		{ configOptions		= options
		, configDebug		= elem OptDebug options
		, configThreads		= fromMaybe 1 (takeLast [n | OptThreads n <- options])
		, configInteractive	= elem OptInteractive options }

	runWar config doWar

	return ()


-- Do War -----------------------------------------------------------------------------------------
doWar :: War ()
doWar 
 = do	config	<- ask

	liftIOF
  	 $ do	-- Make a dir for our temp files if need be
		tmpExists	<- dirExists "/tmp/war"
		when (not tmpExists)
	 	 $ system $ "mkdir /tmp/war"

	-- All the starting test directories
	let testDirs
		= concat
		$ [dirs | OptTestDirs dirs <- configOptions config]

	-- Get all the tests in these directories
	backNodes	<- liftM concat 
			$  mapM  getTestsInDir testDirs

	-- Build a work graph of all the tests
	let workGraph	= WorkGraph.fromBackNodes
			$ backNodes
	
	-- Do it!
	dispatchTests workGraph

	return ()


-- Get Tests --------------------------------------------------------------------------------------
-- Look for tests in this directory
getTestsInDir :: DirPath -> War [(Test, BackNode Test)]
getTestsInDir dirPath
 = do	debugLn $ "- Looking for tests in " ++ dirPath

	-- See what files are there
	files	<- lsFilesIn dirPath

	-- Build and run executables if we have a Main.ds
	let mTestsBuild
		= justWhen (any (isSuffixOf "/Main.ds") files)
		$ let t1	= TestBuildMain (dirPath ++ "/Main.ds")
		      t2	= TestRunBinary (dirPath ++ "/Main.bin")
	 	  in  [ (t1, BackNode [])
		      , (t2, BackNode [t1]) ]

	-- If we ran an executable, and we have a stdout check file
	--	then check the executable's output against it
	let mTestStdout
		| Just [ _, (t2, _) ]	<- mTestsBuild
		, any (isSuffixOf "/Main.stdout.check") files
		= let t3	= TestDiff
					(dirPath ++ "/Main.stdout.check")
					(dirPath ++ "/Main.stdout")
		  in Just [ (t3, BackNode [t2]) ]
		  
		| otherwise
		= Nothing

	-- Recurse into directories
	dirs	 <- lsDirsIn dirPath
	dirTests <- mapM getTestsInDir dirs

	-- 
	return	$  (concat $ catMaybes [mTestsBuild, mTestStdout])
		++ (concat $ dirTests)

justWhen :: Bool -> a -> Maybe a
justWhen True  x	= Just x
justWhen False _	= Nothing
		

-- DispatchTests ----------------------------------------------------------------------------------
type TestWorker	= Worker Test TestResult

dispatchTests :: WorkGraph Test -> War ()
dispatchTests graph
 = do	
	-- make all the worker threads
	config	<- ask
	let makeWorker 
	     = do sVar	<- newEmptyMVar
		  rVar	<- newEmptyMVar
		  tid	<- forkOS $ workerAction config sVar rVar
		  return $ Worker tid False sVar rVar

	config	<- ask
	workers	<- liftM  Set.fromList
		$  liftIO 
		$  replicateM (configThreads config) makeWorker

	-- start the processing loop
	dispatchTests_cmd 
		workers 
		graph 
		Set.empty	-- ignore 
		[] 		-- pref
		Set.empty	-- running tests

dispatchTests_cmd workers graph tsIgnore tsPref tsRunning
	
	-- We've finished all the tests, 
	--	so our work here is done.
	| WorkGraph.null graph
	= return ()

	-- Abort the run if there is any user input.
	| otherwise
	= do	ready	<- liftIO $ hReady stdin
		if ready
	 	 then	liftIO $ exitSuccess
	 	 else	dispatchTests_send workers graph tsIgnore tsPref tsRunning

dispatchTests_send :: Set TestWorker -> WorkGraph Test -> Set Test -> [Test] -> Set Test -> War ()
dispatchTests_send workers graph tsIgnore tsPref tsRunning
  = do	
	-- look for a free worker
	mFreeWorker	<- liftIO 
			$ takeFirstFreeWorker 
			$ Set.toList workers
	
	let result
		-- If there are no free workers then wait for one to finish.
		| Nothing	<- mFreeWorker
		= do	liftIO $ threadDelay 10000	-- 10ms
			dispatchTests_recv workers graph tsIgnore tsPref tsRunning

		-- Try to find a test to send.
		--	there is guaranteed to be a test in the graph
		| Just worker		<- mFreeWorker
		, mTestGraphChildren	<- WorkGraph.takeWorkPrefNot 
						graph 
						tsPref 
						tsRunning
		= case mTestGraphChildren of

			Just (test, children, graph', tsPref')

			 -- There's an available test, but it's in the ignore set.
			 -- 	Skip over it and ignore all its children as well.
			 | Set.member test tsIgnore
			 -> do	liftIO 	$ putStr 
					$ pprResult test (Left TestIgnore) ++ "\n"

				-- Check if any worker have finished in the mean-time
				let tsIgnore'	= Set.union tsIgnore (Set.fromList children)
				dispatchTests_recv workers graph' tsIgnore' tsPref' tsRunning

			 -- There's an available test, and its not in the ignore set,
			 --	and we've got a free worker -- so sent it out.
			 | otherwise
			 -> do	
--				liftIO $ putStr $ "sending test " ++ show test ++ "\n"
				liftIO $ putMVar (workerTestVar worker) (test, children)

				-- Mark the worker as currently busy
				let workers'
					= Set.insert (worker { workerIsBusy = True })
					$ Set.delete worker 
					$ workers

				-- Mark the sent test as currently running
				let tsRunning'	= Set.insert test tsRunning

				-- check if any workers have finished in the mean-time
				dispatchTests_recv workers' graph' tsIgnore tsPref' tsRunning'


			-- We have a free worker, and there are tests in the graph
			--	but none of them are available to be sent at the moment.
			--	We'll need to wait for some workers to finish what they're currently doing
			Nothing
			 ->	dispatchTests_recv workers graph tsIgnore tsPref tsRunning

	result

dispatchTests_recv :: Set TestWorker -> WorkGraph Test -> Set Test -> [Test] -> Set Test -> War ()
dispatchTests_recv workers graph tsIgnore tsPrefs tsRunning
 = do	mWorkerResult	<- liftIO 
			$  takeFirstWorkerResult 
			$  Set.toList workers

	let cont
		-- no workers have a result, wait for a bit then loop again
		| Nothing	<- mWorkerResult
		= do	dispatchTests_cmd workers graph tsIgnore tsPrefs tsRunning

		-- the worker's test failed
		| Just (worker, test, tsChildren, result)	<- mWorkerResult
		= do
			-- print the result
			liftIO $ putStr $ pprResult test result ++ "\n"

			-- mark the worker as free again
			let workers'	= Set.insert (setWorkerAsFree worker) workers	
			let tsRunning'	= Set.delete test tsRunning

			case result of
			 Left err -> do
				-- ignore all the children of this test
				let tsIgnore'	= Set.union tsIgnore (Set.fromList tsChildren)
 	 			dispatchTests_cmd workers' graph tsIgnore' tsPrefs tsRunning'

			 Right _ -> do
				-- prefer to run the tests children
				let tsPrefs'	= tsPrefs ++ tsChildren
			  	dispatchTests_cmd workers' graph tsIgnore tsPrefs' tsRunning'

	cont


-- | The worker slave action.
workerAction 
	:: Config
	-> MVar (Test, [Test]) 			-- ^ MVar to receive tests to run
	-> MVar (Test, [Test], TestResult) 	-- ^ MVar to write results to
	-> IO ()

workerAction config vTest vResult
 = do	tid	<- myThreadId

	-- wait for a test to arrive
	(test, tsChildren)	
		<- takeMVar vTest

	-- run the test
--	putStr 	$  "Thread " ++ show tid ++ " runing " ++ show test ++ "\n"
	result	<- runWar config (runTest test)

--	putStr 	$  "Thread " ++ show tid ++ " posting result" ++ "\n"

	-- post the result back to the master
	putMVar vResult (test, tsChildren, result)

--	putStr 	$  "Thread " ++ show tid ++ " post done" ++ "\n"
	
	-- loop it
	workerAction config vTest vResult
	
	
-- Run Test ----------------------------------------------------------------------------------------
-- | Run a single test
runTest :: Test -> War TestWin
runTest test
 = case test of
	TestBuildMain{}	-> testBuildMain test
	TestRunBinary{}	-> testRunBinary test
	TestDiff{}	-> return TestWinDiffOk


-- Pretty -------------------------------------------------------------------------------------------
-- | Pretty print the result of a test
pprResult :: Test -> TestResult -> String
pprResult test result
 = let	sTest		= pprTest test
	sResult		= case result of
				Left  TestIgnore -> "ignored"

				Left  err	 
				  -> setMode [Bold, Foreground Red] 
				  ++ "failed  " ++ "(" ++ pprTestFail err ++ ")"
				  ++ setMode [Reset]

				Right testWin	 
				  -> pprTestWinColor testWin
  in	sTest ++ sResult


