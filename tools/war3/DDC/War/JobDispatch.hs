
module DDC.War.JobDispatch
	(dispatchJob)
where
import Util.Terminal.VT100
import DDC.War.Job.Compile
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
	JobCompile{}	-> jobCompile job
	JobRun{}	-> jobRun  job
	JobDiff{}	-> jobDiff job


pprJobResult :: Int -> Bool -> Job -> [Aspect] -> Doc
pprJobResult width useColor job aspects
 = case job of

	-- compile should have succeeded, but didn't.
	JobCompile{}
	 | elem AspectUnexpectedFailure aspects
	 ->  padL width (ppr $ jobFile job) 
	 <> (padL 10 $ text $ jobWayName job)
	 <>  text "make   "
	 <> pprAsColor useColor Red (text "compile fail")
	
	-- compile should have failed, but didn't.
	JobCompile{}
	 | elem AspectUnexpectedSuccess aspects
	 ->  padL width (ppr $ jobFile job) 
	 <> (padL 10 $ text $ jobWayName job)
	 <>  text "make   "
	 <> pprAsColor useColor Red (text "unexpected success")

	-- compile did was was expected of it.
	JobCompile{}
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

	-- TODO: Handle run failure.
	
	-- diffed files were different.
	JobDiff{}
   	 | Just AspectDiff{}		<- takeAspectDiff aspects
	 ->  padL width (ppr $ jobFile job)
	 <> (padL 10 $ text $ jobWayName job)
	 <>  text "diff   "
	 <> pprAsColor useColor Red (text "failed")

	-- diffed files were identical, all ok.
	JobDiff{}
	 ->  padL width (ppr $ jobFile job)
	 <> (padL 10 $ text $ jobWayName job)
	 <>  text "diff   "
	 <>  text "ok"


pprAsColor :: Bool -> Color -> Doc -> Doc
pprAsColor True color doc
	=  text (setMode [Bold, Foreground color]) 
	<> doc
	<> text (setMode [Reset])

pprAsColor False _ doc
	= doc