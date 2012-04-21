
module DDC.War.Job.Run
	(jobRun)
where
import DDC.War.Job
import DDC.War.Result
import BuildBox.Command.File
import BuildBox.Command.System
import BuildBox.Build.Benchmark
import BuildBox


-- | Run a binary
jobRun :: Job -> Build [Result]
jobRun (JobRun	testName _wayName _fileName
		mainBin mainRunOut mainRunErr)
 = do	needs mainBin
 
	-- Run the binary.
	(time, (code, strOut, strErr))
	 <- timeBuild
	 $  systemTee False mainBin ""

	-- Write its output to files.
	atomicWriteFile mainRunOut strOut
	atomicWriteFile mainRunErr strErr
	
	return [ResultSuccess] -- [ResultAspect $ Time TotalWall `secs` (fromRational $ toRational time)]
	
