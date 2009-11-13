
-- | Handles a test that defines a Main.sh
--
module Test.Shell
	(testShell)
where

import Test.TestResult
import Test.TestFail
import Test.TestWin
import War
import Command
import Config

import Data.List
import System.Time
import System.Directory

-- | Build a program starting from a Main.ds file
testShell :: Test -> Way -> War TestWin
testShell test@(TestShell mainSH) way
 | isSuffixOf "Main.sh" mainSH
 = do	debugLn $ "* TestShell " ++ mainSH

	let mainDir	= take (length mainSH - length "Main.sh") mainSH

	-- the full path name of the test file
	let mainSH	= mainDir ++ "Main.sh"
	let mainBase	= take (length mainSH - length ".sh") mainSH

	-- where to put the execute logs
	let mainExOut	= mainBase ++ ".execute.stdout"
	let mainExErr	= mainBase ++ ".execute.stderr"

	curDir		<- io $ getCurrentDirectory

	-- run the script
	let cmdBuild	= "sh "   ++ mainSH
			++ " " 	  ++ mainDir
			++ " > "  ++ mainExOut
			++ " 2> " ++ mainExErr
				
	debugLn $ "  * cmd = " ++ cmdBuild

	exTime	<- catchTestIOF (timeIOF_ $ system $ cmdBuild)
			(\ioFail -> TestFailShell
				{ testFailIOFail	= ioFail
				, testFailOutFile	= mainExOut
				, testFailErrFile	= mainExErr })

	return TestWinShell
		{ testWinTime = exTime }
	

