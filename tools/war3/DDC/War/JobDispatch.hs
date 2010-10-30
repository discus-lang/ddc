
module DDC.War.JobDispatch
	(dispatchJob)
where
import DDC.War.Job.CompileHS
import DDC.War.Job.Compile
import DDC.War.Job.Run
import DDC.War.Job.Diff
import DDC.War.Job
import DDC.War.Aspect
import DDC.War.Config
import DDC.War.Pretty
import BuildBox

dispatchJob :: Config -> Job -> Build [Aspect]
dispatchJob config job
 = do	aspects	<- dispatchJob' job

	let useColor	= not $ configBatch config
	let width	= configFormatPathWidth config
	outLn 	$ text " * " 
		<> pprJobResult width useColor job aspects

	return aspects


dispatchJob' :: Job -> Build [Aspect]
dispatchJob' job
 = case job of
	JobCompile{}	-> jobCompile	job
	JobCompileHS{}	-> jobCompileHS	job
	JobRun{}	-> jobRun	job
	JobDiff{}	-> jobDiff 	job


