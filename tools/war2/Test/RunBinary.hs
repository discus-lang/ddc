
module Test.RunBinary
	(testRunBinary)
where

import War
import Test
import Command
import Data.List
import System.Time

testRunBinary :: Test -> War ClockTime
testRunBinary test@(TestRunBinary mainBin)
 | isSuffixOf (".bin") mainBin
 = do	debugLn	$ "* TestRunBinary " ++ mainBin

	-- where to put the run logs
	let mainBase	= take (length mainBin - length ".bin") mainBin
	let mainRunOut	= mainBase ++ ".run.out"
	let mainRunErr	= mainBase ++ ".run.err"

	-- run the test
	let cmdRun	= mainBin
			++ " > "  ++ mainRunOut
			++ " 2> " ++ mainRunErr
		
	debugLn $ "  * cmd = " ++ cmdRun
	runTime
	  <- catchTestIOF (timeIOF_ $ system $ cmdRun)
			(\ioFail -> TestFailInfo test
					[ TestInfoCommand cmdRun
					, TestInfoIOFail  ioFail
					, TestInfoOutFile mainRunOut
					, TestInfoErrFile mainRunErr ])

	return runTime
