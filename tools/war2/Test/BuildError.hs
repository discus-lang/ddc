
-- | Build a program and expect compilation to fail.

module Test.BuildError
	(testBuildError)
where

import Test.TestResult
import Test.TestFail
import Test.TestWin
import War
import Command
import Config

import Util
import Util.FilePath

import Data.List
import System.Time

import Control.Monad.Error


-- | Build a program starting from a Main.ds file
testBuildError :: Test -> Way -> War TestWin
testBuildError test@(TestBuildError mainDS) way
 | isSuffixOf "Main.ds" mainDS
 = do	debugLn $ "* TestBuildError " ++ mainDS 

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
			++ " " 	  ++ (catInt " " $ wayOptsComp way)
			++ " > "  ++ mainCompOut
			++ " 2> " ++ mainCompErr
				
	debugLn $ "  * cmd = " ++ cmdBuild
	result		<- tryWar (liftIOF $ timeIOF_ $ system $ cmdBuild)

	case result of

	 -- compilation failed as expected
	 Left err	
	  -> return TestWinBuildError

	 -- compilation succeeded, but it shouldn't have
	 Right err
	  -> throwError TestFailBuildSuccess
			 { testFailOutFile	= mainCompOut
			 , testFailErrFile	= mainCompErr }

