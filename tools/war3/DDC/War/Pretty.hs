
module DDC.War.Pretty
	(pprJobResult)
where
import DDC.War.Job
import DDC.War.Result
import Util.Terminal.VT100
import BuildBox
import System.FilePath

pprJobResult 
	:: Int 		-- ^ Width of reports.
	-> Bool 	-- ^ Whether to use color in reports.
	-> FilePath	-- ^ Working directory to show test files relative to.
	-> Job 		-- ^ Job to pretty print.
	-> [Result]	-- ^ Returned results of job.
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
	 | or $ map isResultUnexpectedFailure aspects
	 -> pprResult (jobFile job) "compile" 
		Red	(text "failed")
		
	-- compile should have failed, but didn't.
	JobCompile{}
	 | or $ map isResultUnexpectedSuccess aspects
	 -> pprResult (jobFile job) "compile" 
		Red	(text "unexpected success")
	
	-- compile did was was expected of it.
	JobCompile{}
	 | Just time	<- takeResultTime aspects 
	 -> pprResult (jobFile job) "compile" 
		Blue	(text "time" <> (parens $ padR 7 $ ppr time))


	-- CompileHS ----------------------------
	JobCompileHS{}
	 | Just time	<- takeResultTime aspects
	 -> pprResult (jobFile job) "compile" 
		Black	(text "time" <> (parens $ padR 7 $ ppr time))
		
	-- Run ----------------------------------
	-- run was ok.
	JobRun{}
	 | Just time	<- takeResultTime aspects
	 -> pprResult (jobFileBin job) "run"
		Green	(text "time" <> (parens $ padR 7 $ ppr time))

	-- TODO: Handle run failure.
	
	-- Shell --------------------------------
	JobShell{}
	 | or $ map isResultUnexpectedFailure aspects
	 -> pprResult (jobShellSource job) "shell"
		Red 	(text "failed")

	 | or $ map isResultUnexpectedSuccess aspects
	 -> pprResult (jobShellSource job) "shell"
		Red 	(text "unexpected success")

	 | Just time	<- takeResultTime aspects
	 -> pprResult (jobShellSource job) "shell"
		Black 	(text "time" <> (parens $ padR 7 $ ppr time))
	
	-- Diff ---------------------------------
	-- diffed files were different.
	JobDiff{}
   	 | Just _		<- takeResultDiff aspects
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