
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

testRun :: Test -> Way -> War TestWin
testRun test@(TestRun mainBin) way
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
		runTime
	  	  <- catchTestIOF 
			(timeIOF_ $ system $ cmdRun)
			(\ioFail -> TestFailRun
					{ testFailIOFail	= ioFail
					, testFailOutFile	= mainRunOut
					, testFailErrFile	= mainRunErr })
		
		return TestWinRun
			{ testWinTime	= runTime }
