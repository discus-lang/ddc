
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

import Dispatch
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
	dispatch workGraph

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
dispatch :: WorkGraph Test -> War ()
dispatch graph
 = do	
	config	<- ask

	-- Print out test results
	let hookFinished test result
		= putStr $ pprResult test result ++ "\n"

	-- Print tests that get ignored because their parents failed
	let hookIgnored test
		= putStr $ pprResult test (Left TestIgnore) ++ "\n"
		
	-- Check if a test failed
	let resultFailed result
		= case result of 
			Left _ -> True
			Right _ -> False

	liftIO $ dispatchWork 
			hookFinished
			hookIgnored
			resultFailed
			graph
			(configThreads config)
			(workerAction config)

-- | The dispatch worker action
workerAction 
	:: Config
	-> DispatchAction Test TestResult
	
workerAction config vTest vResult
 = do	tid	<- myThreadId

	-- wait for a test to arrive
	(test, tsChildren)	
		<- takeMVar vTest

	-- run the test
	result	<- runWar config (runTest test)

	-- post the result back to the master
	putMVar vResult (test, tsChildren, result)
	
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
 = let	sTest	= pprTest test
	sResult	= case result of
			Left  TestIgnore -> "ignored"

			Left  err	 
			  -> setMode [Bold, Foreground Red] 
			  ++ "failed  " ++ "(" ++ pprTestFail err ++ ")"
			  ++ setMode [Reset]

			Right testWin	 
			  -> pprTestWinColor testWin
  in	sTest ++ sResult


