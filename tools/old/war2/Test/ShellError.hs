
-- | Handles a test that defines a Main.sh
--
module Test.ShellError
	(testShellError)
where
import Test.TestResult
import Test.TestFail
import Test.TestWin
import War
import Command
import Config
import Data.List		(isSuffixOf)
import System.Directory
import Control.Monad.Error
import System.FilePath

-- | Build a program starting from a Main.ds file
testShellError :: Test -> Way -> War TestWin
testShellError test@(TestShellError mainSH) way
 | isSuffixOf "Main.sh" mainSH
 = do	debugLn $ "* TestShellError " ++ mainSH

	let mainDir	= take (length mainSH - length "Main.sh") mainSH

	-- the full path name of the test file
	let mainSH	= mainDir ++ "Main.sh"

	-- where to put the execute logs
	let mainExOut	= replaceExtension mainSH ".execute.stdout"
	let mainExErr	= replaceExtension mainSH ".execute.stderr"

	curDir		<- io $ getCurrentDirectory

	-- run the script
	let cmdBuild	= "sh "   ++ mainSH
			++ " " 	  ++ mainDir
			++ " > "  ++ mainExOut
			++ " 2> " ++ mainExErr
				
	debugLn $ "  * cmd = " ++ cmdBuild
	
	result	<- tryWar (liftIOF $ timeIOF_ $ system $ cmdBuild)

	case result of

	 -- execution failed, as expected
	 Left err 	-> return TestWinShellError

	 -- execution succeeded, but it shouldn't have
	 Right err
	  -> throwError TestFailShellSuccess
			{ testFailOutFile	= mainExOut
			, testFailErrFile	= mainExErr }
