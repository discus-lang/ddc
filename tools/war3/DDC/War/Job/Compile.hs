
module DDC.War.Job.Compile
	(jobCompile)
where
import DDC.War.Job
import DDC.War.Result
import BuildBox
import System.FilePath
import System.Directory
import Data.ListUtil
import Control.Monad


-- | Compile a Disciple source file.
--   TODO: Take a parameter saying whether it's alowd to fail with error.
--         Don't just duplicate the code again like in war2.

jobCompile :: Job -> Build [Result]
jobCompile job@(JobCompile
		testName _wayName srcDS optionsDDC optionsRTS
		buildDir mainCompOut mainCompErr
		mMainBin shouldSucceed)

 = do	needs srcDS
	
	-- The directory holding the Main.ds file.
	let (srcDir, srcFile)	= splitFileName srcDS
		
	-- Copy the .ds files to the build directory.
	-- This freshens them and ensures we won't conflict with other make jobs
	-- running on the same source files, but in different ways.
	ensureDir buildDir
	sources	<- io
		$  liftM (filter (\f -> isSuffixOf ".ds" f || isSuffixOf ".build" f))
		$  lsFilesIn srcDir

	qssystem $ "cp " ++ (catInt " " sources) ++ " " ++ buildDir

	-- The copied version of the root source file.
	let srcCopyDS	= buildDir ++ "/" ++ srcFile
	srcCopyDS'	<- io $ canonicalizePath srcCopyDS

	ddcBin'		<- io $ canonicalizePath "bin/ddc"
		
	-- Do the compile.
	let compile
		| Just mainBin	<- mMainBin
		= do	
			mainBin'	<- io $ canonicalizePath mainBin

			-- If there is an existing binary then remove it.
			qssystem $ "rm -f " ++ mainBin'

			-- Build the program.
	 		runTimedCommand 
	 		 $ io $ systemTeeIO False 
				(ddcBin'
				++ " -v -make "	++ srcCopyDS'
				++ " -o "	++ mainBin'
				++ " " 		++ catInt " " optionsDDC
				++ " +RTS "	++ catInt " " optionsRTS)
				""


		-- Compile the program.
		| otherwise
		=	runTimedCommand 
	 		 $ io $ systemTeeIO False
				(ddcBin'
				++ " -c "	++ srcCopyDS'
				++ " " 		++ catInt " " optionsDDC
				++ " +RTS "	++ catInt " " optionsRTS)
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

	return (ResultAspect (Time TotalWall `secs` (fromRational $ toRational time)) 
		: result)
	