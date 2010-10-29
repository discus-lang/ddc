
module DDC.War.Job.Compile
	(jobCompile)
where
import DDC.War.Aspect
import DDC.War.Job
import BuildBox
import BuildBox.IO.File
import BuildBox.IO.Directory
import System.FilePath
import Util.Data.List
import Control.Monad


-- | Compile a Disciple source file.
--   TODO: Take a parameter saying whether it's alowd to fail with error.
--         Don't just duplicate the code again like in war2.

jobCompile :: Job -> Build [Aspect]
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
		$  liftM (filter (\f -> isSuffixOf ".ds" f))
		$  lsFilesIn srcDir

	qssystem $ "cp " ++ (catInt " " sources) ++ " " ++ buildDir

	-- The copied version of the root source file.
	let srcCopyDS	= buildDir ++ "/" ++ srcFile
		
	-- Do the compile.
	let compile
		| Just mainBin	<- mMainBin
		= do	
			-- If there is an existing binary then remove it.
			qssystem $ "rm -f " ++ mainBin

			-- Build the program.
	 		runTimedCommand 
	 		 $ io $ systemTeeIO False
				("bin/ddc"
				++ " -make "	++ srcCopyDS
				++ " -o "	++ mainBin
				++ " " 		++ catInt " " optionsDDC
				++ " +RTS "	++ catInt " " optionsRTS)
				""
		-- Compile the program.
		| otherwise
		=	runTimedCommand 
	 		 $ io $ systemTeeIO False
				("bin/ddc"
				++ " -c "	++ srcCopyDS
				++ " " 		++ catInt " " optionsDDC
				++ " +RTS "	++ catInt " " optionsRTS)
				""

	(time, (code, strOut, strErr))
		<- compile
	
	-- Decide if it was supposed to succeed or fail.
	let result
		| shouldSucceed
		, ExitFailure _	<- code	
		= [AspectUnexpectedFailure]

		| not shouldSucceed
		, ExitSuccess	<- code
		= [AspectUnexpectedSuccess]
		
		| otherwise
		= []
			
	io $ atomicWriteFile mainCompOut strOut
	io $ atomicWriteFile mainCompErr strErr

	return (AspectTime time : result)
	