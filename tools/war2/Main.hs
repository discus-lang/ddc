
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

import qualified WorkGraph	as WorkGraph
import WorkGraph		(WorkGraph)

import qualified BackGraph	as BackGraph
import BackGraph		(BackNode(..))

import qualified Data.Map	as Map
import Data.Map			(Map)
	
import qualified Data.Set	as Set
import Data.Set			(Set)

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
		, configThreads		= fromMaybe 0 (takeLast [n | OptThreads n <- options])
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
	let workGraph	= WorkGraph.buildWorkGraphFromBackNodes
			$ backNodes
	
	-- Do it!
	dispatchTests 4 workGraph

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
type TestWorker	= Worker Test (Test, TestResult)

dispatchTests :: Int -> WorkGraph Test -> War ()
dispatchTests jobs graph
 = do	masterTid	<- liftIO $ myThreadId
	liftIO $ putStr $ "Master thread is " ++ show masterTid ++ "\n"

	-- make all the worker threads
	let makeWorker 
	     = do sVar	<- newEmptyMVar
		  rVar	<- newEmptyMVar
		  tid	<- forkOS $ workerAction sVar rVar
		  return $ Worker tid False sVar rVar

	workers	<- liftM  Set.fromList
		$  liftIO 
		$  replicateM jobs makeWorker

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
		= dispatchTests_recv workers graph tsIgnore tsPref tsRunning

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

				-- try and find another test.
				let tsIgnore'	= Set.union tsIgnore (Set.fromList children)
				dispatchTests_send workers graph' tsIgnore' tsPref' tsRunning

			 -- There's an available test, and its not in the ignore set,
			 --	and we've got a free worker -- so sent it out.
			 | otherwise
			 -> do	
				liftIO $ putStr $ "sending test " ++ show test ++ "\n"
				liftIO $ putMVar (workerTestVar worker) test

				-- Mark the worker as currently busy
				let workers'
					= Set.insert (worker { workerIsBusy = True })
					$ Set.delete worker 
					$ workers

				-- Mark the sent test as currently running
				let tsRunning'	= Set.insert test tsRunning

				-- try to send some more tests
				dispatchTests_send workers' graph' tsIgnore tsPref' tsRunning'


			-- We have a free worker, and there are tests in the graph
			--	but none of them are available to be sent at the moment.
			--	We'll need to wait for some workers to finish what they're currently doing
			Nothing
			 ->	dispatchTests_recv workers graph tsIgnore tsPref tsRunning

	result

dispatchTests_recv :: Set TestWorker -> WorkGraph Test -> Set Test -> [Test] -> Set Test -> War ()
dispatchTests_recv workers graph tsIgnore tsPref tsRunning
 = do	liftIO $ putStr $ "Receiving\n"

	mWorkerResult	<- liftIO 
			$  takeFirstWorkerResult 
			$  Set.toList workers

	let cont
		-- no workers have a result, wait for a bit then try again
		| Nothing	<- mWorkerResult
		= do	liftIO $ threadDelay 10000000
			dispatchTests_recv workers graph tsIgnore tsPref tsRunning

		-- the worker's test failed
		| Just (worker, (test, Left err))	<-mWorkerResult
		= let 	-- ignore all the children of this test
--			tsIgnore'	= Set.union tsIgnore (Set.fromList children)
			tsIgnore'	= tsIgnore  
   	 
			-- mark the worker as free again
			workers'	= workers			
			tsRunning'	= Set.delete test tsRunning

 	 	  in  	dispatchTests_cmd workers' graph tsIgnore' tsPref tsRunning'

		-- the worker's test succeeded
		| Just (worker, (test, Right res))	<- mWorkerResult
		= let	
			-- prefer to run the tests children
			workers'	= workers
			tsRunning'	= Set.delete test tsRunning

		  in	dispatchTests_cmd workers' graph tsIgnore tsPref tsRunning'

	cont




-- | The worker slave action.
workerAction 
	:: MVar Test 			-- ^ MVar to receive tests to run
	-> MVar (Test, TestResult) 	-- ^ MVar to write results to
	-> IO ()

workerAction vTest vResult
 = do	tid	<- myThreadId

	mTest	<- tryTakeMVar vTest
	case mTest of 
	 Nothing	-> putStr $ "Thread " ++ show tid ++ " waiting\n"
	 Just test	-> putStr $ "Thread " ++ show tid ++ " running " ++ show test ++ "\n"

	threadDelay 1000000

	workerAction vTest vResult

	
-- Run Test ----------------------------------------------------------------------------------------
-- | Run a single test

{-
runTest :: Test -> War (Either TestFail TestWin)
runTest test
 = case test of
	TestBuildMain{}	-> tryWar $ testBuildMain test
	TestRunBinary{}	-> tryWar $ testRunBinary test
	TestDiff{}	-> return $ Right TestWinDiffOk
-}

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


