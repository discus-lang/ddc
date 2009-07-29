
import Config
import Test
import Command
import War
import Timing
import Format

import Util
import Util.Options
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
		, configBatch		= elem OptBatch options }

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
		.  concat
		$  [dirs | OptTestDirs dirs <- configOptions config]

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
	filesAll	<- lsFilesIn dirPath

	-- Dump files also end with .ds 
	--	but we don't want to try and build them...
	let files	= filter 
				(\name -> (not $ isInfixOf ".dump-" name) 
				       && (not $ isInfixOf "-skip"  name)
				       && (not $ isInfixOf "skip-"  name))
				filesAll

	-- Build and run executables if we have a Main.ds
	--	If we have an error.check file then we're expecting it to fail
	let gotMainDS		= any (isSuffixOf "/Main.ds") files
	let gotMainErrorCheck	= any (isSuffixOf "/Main.error.check") files
	let mTestsBuild
		= justWhen (gotMainDS && not gotMainErrorCheck)
		$ let t1	= TestBuild     (dirPath ++ "/Main.ds")
		      t2	= TestRun	(dirPath ++ "/Main.bin")
	 	  in  [ (t1, BackNode [])
		      , (t2, BackNode [t1]) ]

	-- If we have an error.check file then we're expecting failure
	--	Build the program, and assuming it does actually fail,
	--	check the output against the expected.
	let mTestsBuildError
		= justWhen (gotMainDS && gotMainErrorCheck)
		$ let t1	= TestBuildError (dirPath ++ "/Main.ds")
		      t2	= TestDiff       (dirPath ++ "/Main.error.check") (dirPath ++ "/Main.compile.stderr")
		  in  [ (t1, BackNode []) 
		      , (t2, BackNode [t1]) ]

	-- If we ran an executable, and we have a stdout check file
	--	then check the executable's output against it
	let gotMainStdoutCheck	= any (isSuffixOf "/Main.stdout.check") files
	let mTestsStdout
		| Just [ _, (t2, _) ]	<- mTestsBuild
		, gotMainStdoutCheck
		= let t3	= TestDiff
					(dirPath ++ "/Main.stdout.check")
					(dirPath ++ "/Main.stdout")
		  in Just [ (t3, BackNode [t2]) ]
		  
		| otherwise
		= Nothing

	-- If there is no Main.ds then expect every source file that hasn't got an 
	--	associated error.check file to compile successfully.
	let mTestsCompile
		= justWhen (not $ gotMainDS)
		$ [ (TestCompile file, BackNode [])
				| file	<- filter (isSuffixOf ".ds") files 
				, let errorCheckFile	
					= (take (length file - length ".ds") file) ++ ".error.check"
				, not (elem errorCheckFile files)]

	-- If there is not Main.ds file then expect source files with an 
	--	associate error.check file to fail during compilation.
	let mTestsCompileError
		= justWhen (not $ gotMainDS)
		$ concat
		$ [ let t1	= TestCompileError file
		        t2	= TestDiff	   errorCheckFile compileStderr
		    in	[ (t1, BackNode [])
			, (t2, BackNode [t1]) ]
				| file	<- filter (isSuffixOf ".ds") files 
				, let fileBase		= baseNameOfPath file
				, let errorCheckFile	= fileBase ++ ".error.check"
				, let compileStderr	= fileBase ++ ".compile.stderr"
				, elem errorCheckFile files 
		  ]

	let testsHere	= concat 
			$ catMaybes 
				[ mTestsBuild
				, mTestsBuildError
				, mTestsCompile
				, mTestsCompileError 
				, mTestsStdout ]

	-- See what dirs we can recurse into
	dirsAll		<- lsDirsIn dirPath
	
	-- Skip over boring dirs
	let dirs	= filter
				(\name -> (not $ isInfixOf "-skip" name)
				       && (not $ isInfixOf "skip-" name))
				dirsAll

	-- Recurse into directories
	moreTests 
		<- liftM concat
		$  mapM getTestsInDir dirs

	return	$ testsHere ++ moreTests

justWhen :: Bool -> a -> Maybe a
justWhen True  x	= Just x
justWhen False _	= Nothing
		

-- DispatchTests ----------------------------------------------------------------------------------
dispatch :: WorkGraph Test -> War ()
dispatch graph
 = do	
	config	<- ask
		
	-- Check if a test failed
	let resultFailed result
		= case result of 
			Left _ -> True
			Right _ -> False

	liftIO $ dispatchWork 
			(hookFinished config)
			(hookIgnored  config)
			resultFailed
			graph
			(configThreads config)
			(workerAction config)


-- | Called when a test is ignored because one of its parents failed.
hookIgnored :: Config -> Test -> IO ()
hookIgnored config test
 = do 	putStr $ pprResult (not $ configBatch config) test (Left TestIgnore) ++ "\n"
	hFlush stdout

-- | Called when a test has finished running
hookFinished :: Config -> Test -> TestResult -> IO ()
hookFinished config test result

	-- If checked a file against an expected output and it was different,
	--	then ask the user what to do about it.
	| Left (TestFailDiff fileExp fileOut fileDiff)	<- result
	= do	putStr 	$ pprResult (not $ configBatch config) test result ++ "\n"
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
		 $ hookFinishedAsk test result

	| otherwise
	= do	putStr $ pprResult (not $ configBatch config) test result ++ "\n"
		hFlush stdout



hookFinishedAsk :: Test -> TestResult -> IO ()
hookFinishedAsk 
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
			hookFinishedAsk test res

		-- Print the actual output
		| ('a': _)	<- cmd
		= do	str	<- readFile fileOut
			putStr	$  replicate 100 '-' ++ "\n"
			putStr	str
			hookFinishedAsk test res

		-- Update the expected output with the actual one
		| ('u': _)	<- cmd
		= do	System.Cmd.system 
				$ "cp " ++ fileOut ++ " " ++ fileExp

			return ()

		-- Invalid
		| otherwise
		= do	putStr	 $ "Invalid command.\n"
			hookFinishedAsk test res

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

			Left  err	 
			 | color  	-> setMode [Bold, Foreground Red] 
			  	  		++ "failed"
			  	  		++ setMode [Reset]
			 | otherwise 	-> "failed"
				  

			Right testWin	 
			 | color	-> pprTestWinColor testWin
			 | otherwise	-> pprTestWin      testWin

  in	sTest ++ sResult


