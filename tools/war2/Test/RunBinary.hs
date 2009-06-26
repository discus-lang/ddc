
module Test.RunBinary
	(testRunBinary)
where

import War
import Test
import TestFail
import TestWin
import Command
import Data.List
import System.Time

import Control.Monad.Error

testRunBinary :: Test -> War TestWin
testRunBinary test@(TestRunBinary mainBin)
 | isSuffixOf (".bin") mainBin
 = do	debugLn	$ "* TestRunBinary " ++ mainBin

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
				++ " > "  ++ mainRunOut
				++ " 2> " ++ mainRunErr
		
		debugLn $ "  * cmd = " ++ cmdRun
		runTime
	  	  <- catchTestIOF (timeIOF_ $ system $ cmdRun)
				(\ioFail -> TestFailRun
						{ testFailIOFail	= ioFail
						, testFailOutFile	= mainRunOut
						, testFailErrFile	= mainRunErr })

		return TestWinRun
			{ testWinTime	= runTime }
