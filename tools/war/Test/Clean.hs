

module Test.Clean
	(testClean)
where

import Test.TestResult
import Test.TestFail
import Test.TestWin
import War
import Command
import Config

import System.Cmd

testClean :: Test -> Way -> War TestWin
testClean test@(TestClean path) way
 = do	debugLn	$ "* TestClean " ++ path

	io $ System.Cmd.system
		$ "rm -f" 
		++ " " ++ path ++ "/*.di"
		++ " " ++ path ++ "/*.o"
		++ " " ++ path ++ "/*.dump-*"
		++ " " ++ path ++ "/*.ddc.*"
		++ " " ++ path ++ "/*.bin"
		++ " " ++ path ++ "/*.diff"
		++ " " ++ path ++ "/*.stdout"
		++ " " ++ path ++ "/*.stderr"

	return TestWinOk