
import Config
import Test
import Command
import War
import GetTests
import Format
import TestNode
import Util
import Util.Terminal.VT100
import Util.Control.Dispatch
import System.Cmd
import System.Directory
import System.Environment
import System.Exit
import System.IO
import Control.Monad.Reader
import Control.Concurrent
import Util.Data.BackGraph		(BackNode(..))
import Util.Data.WorkGraph		(WorkGraph)
import qualified Util.Data.WorkGraph	as WorkGraph
import qualified Data.Map		as Map
import Util.Options			as Options
import Util.Options.Help		as Options


-- Main -------------------------------------------------------------------------------------------
main :: IO ()
main 
 = do	-- Parse command line options, and exit if they're no good.
	args	<- getArgs
	let (errs, options)	= parseOptions warOptions args
	let help		= makeOptionHelp 30 ["all"] warOptions 

	-- Print command usage if asked for
	when (elem OptHelp options)
	 $ do	putStr $ help ++ "\n"
		exitSuccess

	-- Print errors if there are any
	when (not $ null errs)
	 $ do	putStr $ (catInt "\n" errs) ++ "\n"
		putStr $ help ++ "\n"
		exitFailure

	-- Calculate all the ways we should run the tests
	--	If no options are given for comp or run, then just use
	--	a "normal" way with no options.
	let makeWayPair (name:opts)	= (name, opts)
	    makeWayPair way		= error $ "bad way specification " ++ catInt " " way

	let compWayPairs_	= [makeWayPair opts | OptCompWay opts	<- options ]
	    compWayPairs	= if null compWayPairs_ 
					then [("normal", [])] 
					else compWayPairs_

	    runWayPairs_	= [makeWayPair opts | OptRunWay  opts	<- options ]
	    runWayPairs		= if null runWayPairs_ 
					then [("normal", [])]
					else runWayPairs_

	let ways		= [ WayOpts (compName ++ "-" ++ runName) compOpts runOpts
					| (compName, compOpts)	<- compWayPairs
					, (runName,  runOpts)	<- runWayPairs ]

	when (elem OptDebug options)
	 $ do	putStr	$  "* Ways\n"
			++ unlines [ show way | way <- ways ]
			++ "\n"

	-- Setup war config
	let config
		= Config
		{ configOptions		= options
		, configDebug		= elem OptDebug options
		, configThreads		= fromMaybe 1 (takeLast [n | OptThreads n <- options])
		, configBatch		= elem OptBatch options 
		, configLogFailed	= takeLast [s | OptLogFailed s		<- options]
		, configWays		= ways
		, configClean		= elem OptClean options
		}

	-- run the main program
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
			$  mapM  (getTestsInDir config) testDirs

	-- Build a work graph of all the tests
	let workGraph	= WorkGraph.fromBackNodes
			$ [ (tid, BackNode deps)	| TestNode tid deps	<- backNodes]
	
	-- Do it!
	results		<- dispatch workGraph

	-- Write out a log of failed tests
	when (isJust (configLogFailed config))
	 $ do	let Just logFail	= configLogFailed config
		let testsFailStr
			= concat
			$ [ pprResult False test way (Left fail) ++ "\n"
				| ((test, way), Left fail) <- Map.toList results ]
			

		io $ writeFile logFail testsFailStr

	return ()

		

-- DispatchTests ----------------------------------------------------------------------------------
dispatch 
	:: WorkGraph (Test, Way)		-- tests to run
	-> War (Map (Test, Way) TestResult)	-- test results

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
hookIgnored :: Config -> (Test, Way) -> IO ()
hookIgnored config (test, way)
 = do 	putStr $ pprResult (not $ configBatch config) test way (Left TestIgnore) ++ "\n"
	hFlush stdout

-- | Called when a test has finished running
hookFinished 
	:: Config 				-- ^ war config
	-> MVar (Map (Test, Way) TestResult)	-- ^ test results
	-> (Test, Way) 				-- ^ the test that has finished
	-> TestResult 				-- ^ result of the test
	-> IO ()

hookFinished config varResult (test, way) result
 = do	
	-- add the rest to the result set
	io $ modifyMVar_ 
		varResult 	
		(\m -> return $ Map.insert (test, way) result m)

	hookFinished_ask config test way result
	

hookFinished_ask config test way result

	-- If checked a file against an expected output and it was different,
	--	then ask the user what to do about it.
	| Left (TestFailDiff fileExp fileOut fileDiff)	<- result
	= do	
		putStr 	$ pprResult (not $ configBatch config) test way result ++ "\n"
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
	= do	putStr $ pprResult (not $ configBatch config) test way result ++ "\n"
		hFlush stdout



hookFinished_askDiff :: Test -> TestResult -> IO ()
hookFinished_askDiff 
	test
	res@(Left (TestFailDiff fileExp fileOut fileDiff))
 = do	
	putStr	$  replicate 100 '-' ++ "\n"
		++ "    (ENTER) continue   (e) show expected    (a) show actual\n"
		++ "    (q)     quit       (u) update expected\n"
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
	-> DispatchAction (Test, Way) TestResult
	
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
runTest :: (Test, Way) -> War TestWin
runTest testWay@(test, way)
 = case test of
	TestBuild{}		-> testBuild	    test way
	TestBuildError{}	-> testBuildError   test way
	TestShell{}		-> testShell	    test way
	TestShellError{}	-> testShellError   test way
	TestRun{}		-> testRun	    test way
	TestCompile{}		-> testCompile      test way
	TestCompileError{}	-> testCompileError test way
	TestDiff{}		-> testDiff	    test way
	TestHsBuild{}		-> testHsBuild	    test way
	TestClean{}		-> testClean        test way

-- Pretty -------------------------------------------------------------------------------------------
-- | Pretty print the result of a test
pprResult :: Bool -> Test -> Way -> TestResult -> String
pprResult color test way result
 = let	sTest	= pprTest test
	sWay	= pprWayName way
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

  in	" * " ++ sTest ++ " " ++ Format.padR 15 sWay ++ sResult


