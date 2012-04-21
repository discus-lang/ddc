
module DDC.War.Job.Compile
	(jobCompile)
where
import DDC.War.Job
import DDC.War.Result
import BuildBox.Command.File
import BuildBox.Command.System
import BuildBox.Build.Benchmark
import BuildBox.IO.Directory
import BuildBox
import System.FilePath
import System.Directory
import Data.ListUtil
import Control.Monad


-- | Compile a Disciple source file.
--   TODO: Take a parameter saying whether it's permitted to fail with error.
--         Don't just duplicate the code again like in war2.

jobCompile :: Job -> Build [Result]
jobCompile job@(JobCompile
		testName _wayName srcDS optionsDDC optionsRTS
		buildDir mainCompOut mainCompErr
		mMainBin shouldSucceed)

 = do	needs srcDS
	
	-- The directory holding the Main.ds file.
	let (srcDir, _srcFile)	= splitFileName srcDS
		
	-- Touch the .ds files to the build directory to ensure they're built.
	sources	<- io
		$  liftM (filter (\f -> isSuffixOf ".ds" f || isSuffixOf ".build" f))
		$  lsFilesIn srcDir

	qssystem $ "touch " ++ (catInt " " sources)

	-- ensure the output directory exists
	ensureDir buildDir

	-- Do the compile.
	ddcBin'		<- io $ canonicalizePath "bin/ddc"
	let compile
		| Just mainBin	<- mMainBin
		= do	
			-- If there is an existing binary then remove it.
			qssystem $ "rm -f " ++ mainBin

			-- Build the program.
	 		timeBuild 
	 		 $ systemTee False 
				(ddcBin'
				++ " -v -make "	  ++ srcDS
				++ " -o "	  ++ mainBin
				++ " -outputdir " ++ buildDir
				++ " " 		  ++ catInt " " optionsDDC
				++ " +RTS "	  ++ catInt " " optionsRTS)
				""


		-- Compile the program.
		| otherwise
		= do	timeBuild
	 		 $ systemTee False
				(ddcBin'
				++ " -c "	  ++ srcDS
				++ " -outputdir " ++ buildDir
				++ " " 		  ++ catInt " " optionsDDC
				++ " +RTS "	  ++ catInt " " optionsRTS)
				""

	(time, (code, strOut, strErr))
		<- compile
	
	-- Decide if it was supposed to succeed or fail.
	let result
		| shouldSucceed
		, ExitFailure _	<- code	
		= [ResultUnexpectedFailure]

		| not shouldSucceed
		, ExitSuccess	<- code
		= [ResultUnexpectedSuccess]
		
		| otherwise
		= []
			
	atomicWriteFile mainCompOut strOut
	atomicWriteFile mainCompErr strErr

	return result
--                (ResultAspect (Time TotalWall `secs` (fromRational $ toRational time)) 
--		: result)
	
