
module DDC.War.Job.Run
	(jobRun)
where
import DDC.War.Job
import DDC.War.Aspect
import BuildBox.IO.File
import BuildBox


-- | Run a binary
jobRun :: Job -> Build [Aspect]
jobRun (JobRun	testName _wayName _fileName
		mainBin mainRunOut mainRunErr)
 = do	needs mainBin
	
	-- Run the binary.
	(time, (code, strOut, strErr))
	 <- runTimedCommand 
	 $  io $ systemTeeIO False mainBin ""
	
	-- Write its output to files.
	io $ atomicWriteFile mainRunOut strOut
	io $ atomicWriteFile mainRunErr strErr
	
	return [AspectTime time]
	