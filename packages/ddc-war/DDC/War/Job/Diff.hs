
module DDC.War.Job.Diff
	(jobDiff)
where
import DDC.War.Job
import DDC.War.Result
import BuildBox


-- | Compare two files for differences.
jobDiff :: Job -> Build [Result]
jobDiff (JobDiff testName _wayName 
		fileRef fileOut fileDiff)
 = do	needs fileRef
	needs fileOut
	
	let diff	= "diff"
	
	-- Run the binary.
	(code, strOut, strErr)
	 <- systemTee False 
	 	(diff ++ " " ++ fileRef ++ " " ++ fileOut)
		""
	
	-- Write its output to file.
	atomicWriteFile fileDiff strOut

	if strOut == ""
	 then return []
	 else return [ResultDiff fileRef fileOut fileDiff]

