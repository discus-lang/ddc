
module Test.Run
	(testRun)
where

import Test.TestResult
import Test.TestFail
import Test.TestWin
import War
import Command
import Config
import Timing

import Data.List
import Util.Data.List
import System.Time
import Control.Monad.Error
import qualified System.Cmd	as System.Cmd


testRun :: Test -> Way -> War TestWin
testRun test@(TestRun mainBin (NoShow exitCodeIsSuccess)) way
 | isSuffixOf (".bin") mainBin
 = do	debugLn	$ "* TestRun " ++ mainBin

	exists	<- liftIOF $ fileExists mainBin
	if not exists
	 then	throwError (TestFailMissingFile mainBin)
	 else do
		-- where to put the run logs
		let mainBase	= take (length mainBin - length ".bin") mainBin
		let mainRunOut	= mainBase ++ ".stdout"
		let mainRunErr	= mainBase ++ ".stderr"

		-- run the test
		let cmdRun	= mainBin
				++ " "    ++ (catInt " " $ wayOptsRun way)
				++ " > "  ++ mainRunOut
				++ " 2> " ++ mainRunErr
		
		debugLn $ "  * cmd = " ++ cmdRun
		(exitCode, runTime)	
			<- liftIOF $ io $ timeIO $ System.Cmd.system $ cmdRun
		
		-- Use the fn from the test spec to decide whether the executable 
		--	has returned the correct error code.
		if exitCodeIsSuccess exitCode 
		 then return TestWinRun
			{ testWinTime		= runTime }
		
		 else throwError $ TestFailRun
			{ testFailIOFail	= IOFailCommand cmdRun
			, testFailOutFile	= mainRunOut
			, testFailErrFile	= mainRunErr }
			
