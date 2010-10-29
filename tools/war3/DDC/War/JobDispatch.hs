
module DDC.War.JobDispatch
	(dispatchJob)
where
import Util.Terminal.VT100
import DDC.War.Job.Make
import DDC.War.Job.Run
import DDC.War.Job.Diff
import DDC.War.Job
import DDC.War.Aspect
import DDC.War.Config
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
	JobMake{}	-> jobMake job
	JobRun{}	-> jobRun  job
	JobDiff{}	-> jobDiff job


pprJobResult :: Int -> Bool -> Job -> [Aspect] -> Doc
pprJobResult width useColor job aspects
 = case job of
	JobMake{}
	 | Just (AspectTime time)	<- takeAspectTime aspects 
	 ->  padL width (ppr $ jobFile job) 
	 <> (padL 10 $ text $ jobWayName job)
	 <>  text "make   "
	 <> pprAsColor useColor Blue
		(text "time" <> (parens $ padR 7 $ pprFloatTime $ realToFrac time))
	
	JobRun{}
	 | Just (AspectTime time)	<- takeAspectTime aspects
	 ->  padL width (ppr $ jobFileSrc job)
	 <> (padL 10 $ text $ jobWayName job)
	 <>  text "run    "
	 <> pprAsColor useColor Green 
		(text "time" <> (parens $ padR 7 $ pprFloatTime $ realToFrac time))
	
	JobDiff{}
	 ->  padL width (ppr $ jobFile job)
	 <> (padL 10 $ text $ jobWayName job)
	 <>  text "diff   "


pprAsColor :: Bool -> Color -> Doc -> Doc
pprAsColor True color doc
	=  text (setMode [Bold, Foreground color]) 
	<> doc
	<> text (setMode [Reset])

pprAsColor False _ doc
	= doc