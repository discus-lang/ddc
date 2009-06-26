
-- | Compile a single module
--
module Test.Compile
	(testCompile)
where

import Test.TestResult
import Test.TestFail
import Test.TestWin
import War
import Command

import Util

import Data.List
import System.Time

-- | Build a program starting from a Main.ds file
testCompile :: Test -> War TestWin
testCompile test@(TestCompile srcDS)
 | isSuffixOf ".ds" srcDS
 = do	debugLn $ "* TestCompile " ++ srcDS 

	-- touch the source file to ensure that DDC builds them again
	liftIOF $ system $ "touch " ++ srcDS

	-- the base name of the test file
	let srcBase	= baseNameOfPath srcDS
	let srcObj	= srcBase ++ ".o"

	debugLn $ srcBase
	debugLn $ srcObj

	-- where to put the compile logs
	let srcCompOut	= srcBase ++ ".compile.stdout"
	let srcCompErr	= srcBase ++ ".compile.stderr"
	
	-- if there is an existing objec file then remove it
	liftIOF $ removeIfExists srcObj

	-- compile the file
	let cmdBuild	= "bin/ddc"
			++ " -c " ++ srcDS
			++ " > "  ++ srcCompOut
			++ " 2> " ++ srcCompErr
				
	debugLn $ "  * cmd = " ++ cmdBuild
	compileTime	
	  <- catchTestIOF (timeIOF_ $ system $ cmdBuild)
			(\ioFail -> TestFailCompile
					{ testFailIOFail	= ioFail
					, testFailOutFile	= srcCompOut
					, testFailErrFile	= srcCompErr })

	return TestWinCompile
		{ testWinTime = compileTime
		, testWinSize = 0 }
		
		
baseNameOfPath :: FilePath -> FilePath
baseNameOfPath path
 = let	dirParts	= chopOnRight '/' path
	dir		= concat $ init dirParts

 	fileParts	= chopOnRight '.' $ last dirParts
	file		= concat $ init fileParts
	
   in	dir ++ "/" ++ file


		