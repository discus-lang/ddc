
module DDC.War.Pretty
	(pprJobResult)
where
import DDC.War.Job
import DDC.War.Aspect
import Util.Terminal.VT100
import BuildBox
import System.FilePath

pprJobResult 
	:: Int 		-- ^ Width of reports.
	-> Bool 	-- ^ Whether to use color in reports.
	-> FilePath	-- ^ Working directory to show test files relative to.
	-> Job 		-- ^ Job to pretty print.
	-> [Aspect]	-- ^ Returned aspects of job.
	-> Doc
	
pprJobResult width useColor workingDir job aspects
 = let	pprResult strFile strAction colorResult docResult 
	 =  padL width (text $ makeRelative workingDir strFile) 
	 <> (padL 10 $ text $ jobWayName job)
	 <> (padL 10 $ text strAction)
	 <> pprAsColor useColor colorResult (docResult)

   in case job of

	-- Compile ------------------------------
	-- compile should have succeeded, but didn't.
	JobCompile{}
	 | elem AspectUnexpectedFailure aspects
	 -> pprResult (jobFile job) "compile" 
		Red	(text "compile fail")
		
	-- compile should have failed, but didn't.
	JobCompile{}
	 | elem AspectUnexpectedSuccess aspects
	 -> pprResult (jobFile job) "compile" 
		Red	(text "unexpected success")
	
	-- compile did was was expected of it.
	JobCompile{}
	 | Just (AspectTime time)	<- takeAspectTime aspects 
	 -> pprResult (jobFile job) "compile" 
		Blue	(text "time" <> (parens $ padR 7 $ pprFloatTime $ realToFrac time))


	-- CompileHS ----------------------------
	JobCompileHS{}
	 -> pprResult (jobFile job) "compile" 
		Black	(text "ok")
		
	-- Run ----------------------------------
	-- run was ok.
	JobRun{}
	 | Just (AspectTime time)	<- takeAspectTime aspects
	 -> pprResult (jobFileBin job) "run"
		Green	(text "time" <> (parens $ padR 7 $ pprFloatTime $ realToFrac time))

	-- TODO: Handle run failure.
	
	-- Diff ---------------------------------
	-- diffed files were different.
	JobDiff{}
   	 | Just AspectDiff{}		<- takeAspectDiff aspects
	 -> pprResult (jobFileOut job) "diff"
		Red	(text "failed") 

	-- diffed files were identical, all ok.
	JobDiff{}
	 ->  pprResult (jobFileOut job) "diff"
		Black	(text "ok")



pprAsColor :: Bool -> Color -> Doc -> Doc
pprAsColor True color doc
	=  text (setMode [Bold, Foreground color]) 
	<> doc
	<> text (setMode [Reset])

pprAsColor False _ doc
	= doc