
module Test.HsBuild
	(testHsBuild)
where
import Test.TestResult
import Test.TestFail
import Test.TestWin
import War
import Command
import Config
import Data.List


-- | Build a program starting from a Main.hs file
testHsBuild :: Test -> Way -> War TestWin
testHsBuild test@(TestHsBuild mainHs) way
 | isSuffixOf "Main.hs" mainHs
 = do	debugLn $ "* TestBuild " ++ mainHs 

	let mainDir	= take (length mainHs - length "Main.hs") mainHs

	-- touch all the .hs files here, and clean up any .hi  to ensure that DDC builds them again
	liftIOF 
         $ do	files	<- lsFilesIn mainDir
		mapM_ (\f -> system $ "touch " ++ f)
			$ filter (isSuffixOf ".hs") files
		
		mapM_ (\f -> system $ "rm " ++ f)
			$ filter (isSuffixOf ".hi") files

	-- the full path name of the test file
	let mainHs	= mainDir ++ "Main.hs"
	let mainBase	= take (length mainHs - length ".hs") mainHs

	-- where to put the compile logs
	let mainCompOut	= mainBase ++ ".compile.stdout"
	let mainCompErr	= mainBase ++ ".compile.stderr"

	-- where to put the compiled binary
	let mainBin	= mainBase ++ ".bin"
	let buildMk	= mainDir ++ "build.mk"

	-- if there is an existing binary then remove it
	liftIOF $ removeIfExists mainBin

	-- remove the generate build.mk file.
	liftIOF $ removeIfExists buildMk

	liftIOF $ genBuildMk buildMk mainBin mainHs

	-- build the test
	let cmdBuild	= "make -f " ++ buildMk
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



genBuildMk :: FilePath -> String -> String -> IOF ()
genBuildMk outfile mainBin mainHs
 = do	let str	= "# Generated Makefile\n\n"
 		++ "include make/build.mk\n\n"
                ++ mainBin ++ " : " ++ mainHs ++ "\n"
                ++ "\t$(GHC) $(GHC_LANGUAGE) $(DDC_PACKAGES) -isrc --make $^ -o $@\n\n"
	io $ writeFile outfile str
