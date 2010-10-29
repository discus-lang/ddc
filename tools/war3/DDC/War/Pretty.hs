
module DDC.War.Pretty
	(pprJobResult)
where
import DDC.War.Job
import DDC.War.Aspect
import Util.Terminal.VT100
import BuildBox


pprJobResult :: Int -> Bool -> Job -> [Aspect] -> Doc
pprJobResult width useColor job aspects
 = case job of

	-- compile should have succeeded, but didn't.
	JobCompile{}
	 | elem AspectUnexpectedFailure aspects
	 ->  padL width (ppr $ jobFile job) 
	 <> (padL 10 $ text $ jobWayName job)
	 <>  text "compile "
	 <> pprAsColor useColor Red (text "compile fail")
	
	-- compile should have failed, but didn't.
	JobCompile{}
	 | elem AspectUnexpectedSuccess aspects
	 ->  padL width (ppr $ jobFile job) 
	 <> (padL 10 $ text $ jobWayName job)
	 <>  text "compile "
	 <> pprAsColor useColor Red (text "unexpected success")

	-- compile did was was expected of it.
	JobCompile{}
	 | Just (AspectTime time)	<- takeAspectTime aspects 
	 ->  padL width (ppr $ jobFile job) 
	 <> (padL 10 $ text $ jobWayName job)
	 <>  text "compile "
	 <> pprAsColor useColor Blue
		(text "time" <> (parens $ padR 7 $ pprFloatTime $ realToFrac time))

	JobRun{}
	 | Just (AspectTime time)	<- takeAspectTime aspects
	 ->  padL width (ppr $ jobFileBin job)
	 <> (padL 10 $ text $ jobWayName job)
	 <>  text "run     "
	 <> pprAsColor useColor Green 
		(text "time" <> (parens $ padR 7 $ pprFloatTime $ realToFrac time))

	-- TODO: Handle run failure.
	
	-- diffed files were different.
	JobDiff{}
   	 | Just AspectDiff{}		<- takeAspectDiff aspects
	 ->  padL width (ppr $ jobFileOut job)
	 <> (padL 10 $ text $ jobWayName job)
	 <>  text "diff    "
	 <> pprAsColor useColor Red (text "failed")

	-- diffed files were identical, all ok.
	JobDiff{}
	 ->  padL width (ppr $ jobFileOut job)
	 <> (padL 10 $ text $ jobWayName job)
	 <>  text "diff    "
	 <>  text "ok"


pprAsColor :: Bool -> Color -> Doc -> Doc
pprAsColor True color doc
	=  text (setMode [Bold, Foreground color]) 
	<> doc
	<> text (setMode [Reset])

pprAsColor False _ doc
	= doc