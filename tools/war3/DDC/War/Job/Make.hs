
module DDC.War.Job.Make
	(jobMake)
where
import DDC.War.Aspect
import DDC.War.Job
import BuildBox
import BuildBox.IO.File
import BuildBox.IO.Directory
import System.FilePath
import Util.Data.List
import Control.Monad


-- | Make a Disciple program.
jobMake :: Job -> Build [Aspect]
jobMake (JobMake 
		testName _wayName mainDS optionsDDC optionsRTS
		buildDir mainBin mainCompOut mainCompErr)

 = do	needs mainDS
		
	-- The directory holding the Main.ds file.
	let srcDir	= takeDirectory mainDS
	
	-- Copy the .ds files to the build directory.
	-- This freshens them and ensures we won't conflict with other make jobs
	-- running on the same source files, but in different ways.
	ensureDir buildDir
	sources	<- io
		$  liftM (filter (\f -> isSuffixOf ".ds" f))
		$  lsFilesIn srcDir

	qssystem $ "cp " ++ (catInt " " sources) ++ " " ++ buildDir

	let mainCopyDS	= buildDir ++ "/" ++ "Main.ds"
		
	-- If there is an existing binary then remove it.
	qssystem $ "rm -f " ++ mainBin

	-- Build the program.
	(time, (code, strOut, strErr))
	 <- runTimedCommand 
	 $  io $ systemTeeIO False
			("bin/ddc"
				++ " -make "	++ mainCopyDS
				++ " -o "	++ mainBin
				++ " " 		++ catInt " " optionsDDC
				++ " +RTS "	++ catInt " " optionsRTS)
			""
	io $ atomicWriteFile mainCompOut strOut
	io $ atomicWriteFile mainCompErr strErr

	return [AspectTime time]
	
