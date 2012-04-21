
module DDC.War.Job.Shell
	(jobShell)
where
import DDC.War.Job
import DDC.War.Result
import BuildBox.Command.File
import BuildBox.Command.System
import BuildBox.Build.Benchmark
import BuildBox


-- | Run a binary
jobShell :: Job -> Build [Result]
jobShell (JobShell testName _wayName
		mainSH sourceDir scratchDir
		mainRunOut mainRunErr
		shouldSucceed)
 = do	needs mainSH
	ensureDir scratchDir
	
	-- Run the binary.
	(time, (code, strOut, strErr))
	 <- timeBuild
	 $  systemTee False 
		("sh " ++ mainSH ++ " " ++ sourceDir ++ " " ++ scratchDir) 
		""
		
	-- Write its output to files.
	atomicWriteFile mainRunOut strOut
	atomicWriteFile mainRunErr strErr
		
	return  -- $  [ ResultAspect $ Time TotalWall `secs` (fromRational $ toRational time)]

		-- check for unexpected failure
		$  (if shouldSucceed && code /= ExitSuccess 
			then [ResultUnexpectedFailure] else [])

		-- check for unexpected success
		++ (if not shouldSucceed && code == ExitSuccess
			then [ResultUnexpectedSuccess] else [])		 	
