
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
import Config
import Util
import System.FilePath


-- | Build a program starting from a Main.ds file
testCompile :: Test -> Way -> War TestWin
testCompile test@(TestCompile srcDS) way
 | isSuffixOf ".ds" srcDS
 = do	debugLn $ "* TestCompile " ++ srcDS 

	-- touch the source file to ensure that DDC builds them again
	liftIOF $ system $ "touch " ++ srcDS

	-- the base name of the test file
	let srcBase	= takeBaseName srcDS
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
			++ " "    ++ (catInt " " $ wayOptsComp way)
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
		
		

		