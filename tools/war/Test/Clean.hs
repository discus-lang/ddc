
module Test.Clean
	(testClean)
where
import Test.TestResult
import Test.TestWin
import War
import Command
import Config
import System.Cmd


testClean :: Test -> Way -> War TestWin
testClean test@(TestClean path) way
 = do	debugLn	$ "* TestClean " ++ path

	io $ System.Cmd.system
		$ "find " ++ path ++ " -type f "
		++ " -name *.di"
		++ " -o -name *.o"
		++ " -o -name *.dump-*"
		++ " -o -name *.ddc.*"
		++ " -o -name *.bin"
		++ " -o -name *.diff"
		++ " -o -name *.stdout"
		++ " -o -name *.stderr"
		++ " -exec rm -f {} \\;"

	return TestWinOk
