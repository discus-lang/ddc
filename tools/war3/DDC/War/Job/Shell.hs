
module DDC.War.Job.Shell
	(jobShell)
where
import DDC.War.Job
import DDC.War.Result
import BuildBox


-- | Run a binary
jobShell :: Job -> Build [Result]
jobShell (JobShell testName _wayName
		mainSH sourceDir scratchDir
		mainRunOut mainRunErr
		shouldSucceed)
 = do	needs mainSH
	
	-- Run the binary.
	(time, (code, strOut, strErr))
	 <- runTimedCommand 
	 $  systemTee False (mainSH ++ " " ++ sourceDir ++ " " ++ scratchDir) ""
		
	-- Write its output to files.
	atomicWriteFile mainRunOut strOut
	atomicWriteFile mainRunErr strErr
	
	return  [ ResultAspect $ Time TotalWall `secs` (fromRational $ toRational time)
		, ResultQuirk  $ (if code == ExitSuccess then QuirkSucceeded else QuirkFailed) ]
		 	
