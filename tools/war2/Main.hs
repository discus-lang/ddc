

import Config
import Test
import TestFail
import TestWin
import Command
import War
import Timing
import Format
import Test.BuildMain
import Test.RunBinary

import Util			(catInt, fromMaybe, takeLast, isSuffixOf, catMaybes)
import Util.Options

import System.Environment
import System.Exit
import System.Time
import Control.Monad.Reader

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

	let workGraph	= WorkGraph.buildWorkGraphFromBackNodes
			$ backNodes
	
--	liftIO $ print backNodes
--	liftIO $ print workGraph

	dispatchTests workGraph

--	results	<- mapM runTest tests

--	liftIO
--	 $ print $ catInt "\n" $ map pprResult results

	return ()


-- Get Tests --------------------------------------------------------------------------------------

justWhen :: Bool -> a -> Maybe a
justWhen True  x	= Just x
justWhen False _	= Nothing


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
		

-- DispatchTests ----------------------------------------------------------------------------------
dispatchTests :: WorkGraph Test -> War ()
dispatchTests graph
	= dispatchTests' graph Set.empty []

dispatchTests' graph tsIgnore tsPref

	-- we've finished all the tests
	| WorkGraph.null graph
	= return ()

 	| (Just (test, children), graph', tsPref')	
			<- WorkGraph.takeWorkPref tsPref graph
	= if Set.member test tsIgnore 

	   -- If a test is in the ignore set then don't run it or its children
	   then do	
		liftIO $ putStr $ pprResult test (Left TestIgnore) ++ "\n"
		let tsIgnore'		= Set.union tsIgnore (Set.fromList children)
		dispatchTests' graph' tsIgnore' tsPref'

	   -- Run this test
	   else do
		testResult	<- runTest test
		liftIO $ putStr $ pprResult test testResult ++ "\n"

		let result
			-- If the test failed, then ignore its children
			| Left err	<- testResult
			= let tsIgnore'	= Set.union tsIgnore (Set.fromList children)
		     	  in  dispatchTests' graph' tsIgnore' tsPref'

			-- If the test succeeded, then prefer to run its children next time 
		 	| Right _	<- testResult
			= dispatchTests' graph' tsIgnore (tsPref' ++ children)

		result

-- Run Test ----------------------------------------------------------------------------------------
runTest :: Test -> War (Either TestFail TestWin)
runTest test
 = case test of
	TestBuildMain{}	-> tryWar $ testBuildMain test
	TestRunBinary{}	-> tryWar $ testRunBinary test
	TestDiff{}	-> return $ Right TestWinDiffOk

-- Pretty -------------------------------------------------------------------------------------------
pprResult :: Test -> Either TestFail TestWin -> String
pprResult test result
 = let	sTest		= pprTest test
	sResult		= case result of
				Left  TestIgnore -> "ignored"
				Left  err	 -> "failed  " ++ "(" ++ pprTestFail err ++ ")"
				Right testWin	 -> pprTestWin testWin
  in	sTest ++ sResult


