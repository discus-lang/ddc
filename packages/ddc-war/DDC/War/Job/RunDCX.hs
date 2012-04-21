
module DDC.War.Job.RunDCX
	(jobRunDCX)
where
import DDC.War.Result
import DDC.War.Job
import BuildBox.Command.File
import BuildBox.Command.System
import BuildBox.Build.Benchmark
import BuildBox
import System.Directory


-- | Compile a Haskell Source File
jobRunDCX :: Job -> Build [Result]
jobRunDCX (JobRunDCX
		testName _wayName srcDCX
		buildDir testRunStdout testRunStderr)

 = do	needs srcDCX

	-- ensure the output directory exists
	ensureDir buildDir

	ddciBin' <- io $ canonicalizePath "bin/ddci-core"

	(time, (code, strOut, strErr))
	  <- timeBuild
	  $  systemTee False
		(ddciBin' ++ " --batch " ++ srcDCX)
		""
	atomicWriteFile testRunStdout strOut
	atomicWriteFile testRunStderr strErr

        let result 
                = case code of
                        ExitFailure _   -> [ResultUnexpectedFailure]
                        _               -> []
                
--	let ftime	= fromRational $ toRational time

	return  $  result 
--	        ++ [ ResultAspect $ Time TotalWall `secs` ftime ]
	
	
