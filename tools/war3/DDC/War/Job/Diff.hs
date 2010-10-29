
module DDC.War.Job.Diff
	(jobDiff)
where
import DDC.War.Job
import DDC.War.Aspect
import BuildBox.IO.File
import BuildBox


-- | Compare two files for differences.
jobDiff :: Job -> Build [Aspect]
jobDiff (JobDiff testName _wayName 
		file fileOut fileDiff)
 = do	needs file
	needs fileOut
	
	-- Run the binary.
	(code, strOut, strErr)
	 <- systemTee False 
	 	("diff " ++ file ++ " " ++ fileOut)
		""
	
	-- Write its output to file.
	io $ atomicWriteFile fileDiff strOut

	if strOut == ""
	 then return []
	 else return [AspectDiff file fileOut fileDiff]
	