
-- | Handles a test that defines a Main module.
--	We compile the module, run the test, then check it's output.
--
module Test.Build 
	(testBuild)
where

import Test.TestResult
import Test.TestFail
import Test.TestWin
import Util.Data.List
import War
import Command
import Config

import Data.List
import System.Time

-- | Build a program starting from a Main.ds file
testBuild :: Test -> Way -> War TestWin
testBuild test@(TestBuild mainDS) way
 | isSuffixOf "Main.ds" mainDS
 = do	debugLn $ "* TestBuild " ++ mainDS 

	let mainDir	= take (length mainDS - length "Main.ds") mainDS

	-- touch all the .ds files here, and clean up any .di  to ensure that DDC builds them again
	liftIOF 
         $ do	files	<- lsFilesIn mainDir
		mapM_ (\f -> system $ "touch " ++ f)
			$ filter (isSuffixOf ".ds") files
		
		mapM_ (\f -> system $ "rm " ++ f)
			$ filter (isSuffixOf ".di") files

	-- the full path name of the test file
	let mainDS	= mainDir ++ "Main.ds"
	let mainBase	= take (length mainDS - length ".ds") mainDS

	-- where to put the compile logs
	let mainCompOut	= mainBase ++ ".compile.stdout"
	let mainCompErr	= mainBase ++ ".compile.stderr"

	-- where to put the compiled binary
	let mainBin	= mainBase ++ ".bin"
	
	-- if there is an existing binary then remove it
	liftIOF $ removeIfExists mainBin

	-- build the test
	let cmdBuild	= "bin/ddc"
			++ " -make " ++ mainDS
			++ " -o " ++ mainBin
			++ " "    ++ (catInt " " $ wayOptsComp way)
			++ " > "  ++ mainCompOut
			++ " 2> " ++ mainCompErr
				
	debugLn $ "  * cmd = " ++ cmdBuild
	compileTime	
	  <- catchTestIOF (timeIOF_ $ system $ cmdBuild)
			(\ioFail -> TestFailBuild
					{ testFailIOFail	= ioFail
					, testFailOutFile	= mainCompOut
					, testFailErrFile	= mainCompErr })

	return TestWinBuild
		{ testWinTime = compileTime
		, testWinSize = 0 }






