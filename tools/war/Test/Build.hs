
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

-- Need these for determining pointerSize
import Foreign.Storable		(sizeOf)
import Foreign.Ptr		(Ptr)
import Foreign.C.Types		(CChar)


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
			++ " +RTS -M" ++ heapLimit ++ " -RTS"   -- Limit heap size
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


-- | Provide a heap size limit for bin/ddc in war tests. DDC on 64 bit
-- systems gets twice as much heap as 32 bit systems because pointers
-- will be 64 bits and a lot of stuff in the heap will be pointers.
--
-- This value is kept conservatively low to act as a canary in the coal
-- mine for regressions that suddenly start using a lot of heap space.
--
-- At the time this was set to 30M for 32 bit systems, compiling
-- test/90-programs/Rover after a 'make cleanWar' was taking 24M of heap
-- space (about 48M on 64 bit systems).
--
heapLimit :: String
heapLimit
 = case pointerSize of
	32	-> "30M"	-- Megabytes
	64	-> "60M"


pointerSize :: Int
pointerSize = 8 * sizeOf (undefined :: Ptr CChar)

