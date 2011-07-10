
module DDC.War.JobDispatch
	(dispatchJob)
where
import DDC.War.Job.CompileHS
import DDC.War.Job.Compile
import DDC.War.Job.Run
import DDC.War.Job.Shell
import DDC.War.Job.Diff
import DDC.War.Job
import DDC.War.Result
import BuildBox

dispatchJob :: Job -> Build [Result]
dispatchJob job
 = case job of
	JobCompile{}	-> jobCompile	job
	JobCompileHS{}	-> jobCompileHS	job
	JobRun{}	-> jobRun	job
	JobShell{}	-> jobShell	job
	JobDiff{}	-> jobDiff 	job


