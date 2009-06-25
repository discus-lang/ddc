

import Config
import Test
import Command
import War
import Timing
import Test.BuildMain
import Test.RunBinary

import Util
import Util.Options

import System.Environment
import System.Exit
import System.Time
import Control.Monad.Reader

	
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

pprResult :: Either TestFail ClockTime -> String
pprResult result
 = case result of
	Left  err	-> show err
	Right testTime	-> pprClockTime testTime

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
	tests	<- liftM concat 
		$  mapM  getTestsInDir testDirs

	results	<- mapM runTest tests

	liftIO
	 $ print $ catInt "\n" $ map pprResult results

	return ()

-- Get Tests --------------------------------------------------------------------------------------
getTestsInDir :: DirPath -> War [TestNode]
getTestsInDir dirPath
 = do	debugLn $ "- Looking for tests in " ++ dirPath

	-- See what files are there
	files	<- lsFilesIn dirPath

	let testsMainDS
		| any (isSuffixOf "Main.ds") files
		= let t1	= [TestBuildMain (dirPath ++ "/Main.ds")] 
		      t2	= [TestRunBinary (dirPath ++ "/Main.bin")]
	 	  in  [ TestNode []   t1
		      , TestNode [t1] t2 ]

	return	testsMainDS


-- DispatchTests ----------------------------------------------------------------------------------
dispatchTests :: Set TestNode -> War ()

dispatchTests inSet deferSet
	| Set.empty testSet
	= return ()

 	| TestNode deps test : _	<- Set.toList testSet
	, not $ any (flip Set.member testSet) deps
	= do	result	<- runTest test
		print $ pprResult result

		let testSet'	= Set.delete test testSet
		dispatchTests 



-- Run Test ----------------------------------------------------------------------------------------
runTest :: Test -> War (Either TestFail ClockTime)
runTest test
 = case test of
	TestBuildMain{}	-> tryWar $ testBuildMain test
	TestRunBinary{}	-> tryWar $ testRunBinary test
 