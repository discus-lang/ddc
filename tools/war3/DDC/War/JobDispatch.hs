
module DDC.War.JobDispatch
	(dispatchJob)
where
import DDC.War.Job.Make
import DDC.War.Job
import DDC.War.Aspect
import BuildBox

dispatchJob :: Job -> Build [Aspect]
dispatchJob job
 = do	aspects	<- dispatchJob' job
	outLn 	$  "dispatch: " ++ show job
		++ " aspects: " ++ show aspects
	return aspects

dispatchJob' :: Job -> Build [Aspect]
dispatchJob' job
 = case job of
	JobMake{}	-> jobMake job
	_		-> error $ "dispatchJob: " ++ show job