
-- | Handles a test that defines a Main module.
--	We compile the module, run the test, then check it's output.
--
module Test.BuildMain 
	(testBuildMain)
where

import War
import Test
import TestFail
import TestWin
import Command
import Data.List
import System.Time

-- | Build a program starting from a Main.ds file
testBuildMain :: Test -> War TestWin
testBuildMain test@(TestBuildMain mainDS)
 | isSuffixOf "Main.ds" mainDS
 = do	debugLn $ "* TestBuildMain " ++ mainDS 

	let mainDir	= take (length mainDS - length "Main.ds") mainDS

	-- touch all the .ds files here to ensure that DDC builds them again
	liftIOF 
         $ do	files	<- lsFilesIn mainDir
		mapM_ (\f -> system $ "touch " ++ f)
			$ filter (isSuffixOf ".ds") files	

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
			++ " > "  ++ mainCompOut
			++ " 2> " ++ mainCompErr
				
	debugLn $ "  * cmd = " ++ cmdBuild
	compileTime	
	  <- catchTestIOF (timeIOF_ $ system $ cmdBuild)
			(\ioFail -> TestFailCompile
					{ testFailIOFail	= ioFail
					, testFailOutFile	= mainCompOut
					, testFailErrFile	= mainCompErr })
	return TestWinBuildMain
		{ testWinTime = compileTime
		, testWinSize = 0 }






