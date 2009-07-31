
import Config
import Test
import Command
import War
import Timing
import Format
import GetTests

import Util
import Util.Options			as Options
import Util.Options.Help		as Options

import Util.Terminal.VT100
import Util.FilePath
import Util.Control.Dispatch
import Util.Control.Dispatch.Worker
import Util.Data.BackGraph		(BackNode(..))
import Util.Data.WorkGraph		(WorkGraph, WorkNode(..))
import System.Cmd
import System.Directory
import System.Environment
import System.Exit
import System.Time
import System.IO
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Util.Data.BackGraph	as BackGraph
import qualified Util.Data.WorkGraph	as WorkGraph
import qualified Data.Map		as Map
import qualified Data.Set		as Set


-- Main -------------------------------------------------------------------------------------------
main :: IO ()
main 
 = do	-- Parse command line options, and exit if they're no good.
	args	<- getArgs
	let (errs, options)	= parseOptions warOptions args
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
		, configBatch		= elem OptBatch options 
		, configLogFailed	= takeLast [s | OptLogFailed s <- options]
		}

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
	 	 $ Command.system $ "mkdir /tmp/war"

	-- All the starting test directories
	testDirs
		<- liftIO
		.  mapM (makeRelativeToCurrentDirectory <=< canonicalizePath)
		$  [dirs | OptTestDir dirs <- configOptions config]

	-- Get all the tests in these directories
	backNodes	<- liftM concat 
			$  mapM  getTestsInDir testDirs

	-- Build a work graph of all the tests
	let workGraph	= WorkGraph.fromBackNodes
			$ backNodes
	
	-- Do it!
	results		<- dispatch workGraph

	-- Write out a log of failed tests
	when (isJust (configLogFailed config))
	 $ do	let Just logFail	= configLogFailed config
		let testsFailStr
			= concat
			$ [ " * " ++ pprTest test ++ " " ++ pprTestFail fail ++ "\n"
				| (test, Left fail) <- Map.toList results ]
			

		io $ writeFile logFail testsFailStr

	return ()

		

-- DispatchTests ----------------------------------------------------------------------------------
dispatch 
	:: WorkGraph Test 		-- tests to run
	-> War (Map Test TestResult)	-- test results

dispatch graph
 = do	
	-- get the config from the War monad
	config	<- ask
		
	-- Map of test results
	varResult	<- io $ newMVar Map.empty

	-- Fn to check if a test failed
	let resultFailed result
		= case result of 
			Left _ -> True
			Right _ -> False

	liftIO $ dispatchWork 
			(hookFinished config varResult)
			(hookIgnored  config)
			resultFailed
			graph
			(configThreads config)
			(workerAction config)

	-- Read back the set of failed tests
	results	<- io $ takeMVar varResult
	return results


-- | Called when a test is ignored because one of its parents failed.
hookIgnored :: Config -> Test -> IO ()
hookIgnored config test
 = do 	putStr $ pprResult (not $ configBatch config) test (Left TestIgnore) ++ "\n"
	hFlush stdout

-- | Called when a test has finished running
hookFinished 
	:: Config 			-- ^ war config
	-> MVar (Map Test TestResult)	-- ^ test results
	-> Test 			-- ^ the test that has finished
	-> TestResult 			-- ^ result of the test
	-> IO ()

hookFinished config varResult test result
 = do	
	-- add the rest to the result set
	io $ modifyMVar_ varResult (\m -> return $ Map.insert test result m)

	hookFinished_ask config test result
	

hookFinished_ask config test result

	-- If checked a file against an expected output and it was different,
	--	then ask the user what to do about it.
	| Left (TestFailDiff fileExp fileOut fileDiff)	<- result
	= do	
		putStr 	$ pprResult (not $ configBatch config) test result ++ "\n"
		putStr	$  "\n"
			++ "-- Output Differs  ----------------------------------------------------------\n"
			++ "   expected file: " ++ fileExp	++	"\n"
			++ "     actual file: " ++ fileOut	++ 	"\n"
			++ replicate 100 '-' ++ "\n"

		str	<- readFile fileDiff
		putStr	str
		hFlush stdout

		-- If we're not in batch mode, ask the user what to do about the failure.
		when (not $ configBatch config)
		 $ hookFinished_askDiff test result

		return ()

	| otherwise
	= do	putStr $ pprResult (not $ configBatch config) test result ++ "\n"
		hFlush stdout



hookFinished_askDiff :: Test -> TestResult -> IO ()
hookFinished_askDiff 
	test
	res@(Left (TestFailDiff fileExp fileOut fileDiff))
 = do	
	putStr	$  replicate 100 '-' ++ "\n"
		++ "    (ENTER) continue   (e) show expected    (a) show actual\n"
		++ "    (q)     quit       (u) update accepted\n"
		++ "\n"
		++ "? "

	hFlush stdout
	cmd	<- hGetLine stdin
	
	let result
		-- Continue	
		| ""		<- cmd
		= return ()

		-- Quit
		| ('q': _)	<- cmd
		= do	exitSuccess

		-- Print the expected output
		| ('e': _)	<- cmd
		= do	str	<- readFile fileExp
			putStr	$  replicate 100 '-' ++ "\n"
			putStr	str
			hookFinished_askDiff test res

		-- Print the actual output
		| ('a': _)	<- cmd
		= do	str	<- readFile fileOut
			putStr	$  replicate 100 '-' ++ "\n"
			putStr	str
			hookFinished_askDiff test res

		-- Update the expected output with the actual one
		| ('u': _)	<- cmd
		= do	System.Cmd.system 
				$ "cp " ++ fileOut ++ " " ++ fileExp

			return ()

		-- Invalid
		| otherwise
		= do	putStr	 $ "Invalid command.\n"
			hookFinished_askDiff test res

	result			


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
	TestBuild{}		-> testBuild	    test
	TestBuildError{}	-> testBuildError   test
	TestShell{}		-> testShell	    test
	TestShellError{}	-> testShellError   test
	TestRun{}		-> testRun	    test
	TestCompile{}		-> testCompile      test
	TestCompileError{}	-> testCompileError test
	TestDiff{}		-> testDiff	    test


-- Pretty -------------------------------------------------------------------------------------------
-- | Pretty print the result of a test
pprResult :: Bool -> Test -> TestResult -> String
pprResult color test result
 = let	sTest	= pprTest test
	sResult	= case result of
			Left  TestIgnore -> "ignored"

			Left  fail
			 | color  	-> setMode [Bold, Foreground Red] 
			  	  		++ pprTestFail fail
			  	  		++ setMode [Reset]
			 | otherwise 	-> pprTestFail fail

			Right testWin	 
			 | color	-> pprTestWinColor testWin
			 | otherwise	-> pprTestWin      testWin

  in	" * " ++ sTest ++ sResult


