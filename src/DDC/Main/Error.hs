
module DDC.Main.Error
	( panic
	, freakout
	, dieWithUserError
	, exitWithUserError)
where
import DDC.Main.Pretty
import DDC.Main.Arg	
import System.Exit
import Debug.Trace
import qualified DDC.Config.Version	as Version


-- | The walls are crashing down.
--	Print a last, dying message and bail out.
panic 	:: Pretty msg PMode
	=> String 		-- ^ Name of module the panic was in.
	-> msg 			-- ^ Message to display.
	-> a

panic  stage msg
	=  error
		( pprStrPlain
		$ "PANIC in " % stage % "\n" 
		%> (msg  % "\nPlease report this bug to the maintainers at:\n"
			 % Version.maintainers))


-- | Something bad has happened, and it's likely to be terminal.
--	Hopefully we can contine on for a bit longer until some other error occurs
--	that gives information about what caused this problem.
freakout 
	:: Pretty msg PMode
	=> String 		-- ^ Bame of module the freakout was in.
	-> msg 			-- ^ Message to display.
	-> a 			-- ^ Value to return
	-> a

freakout stage msg a
	= trace 
		(pprStrPlain
		$ "FREAKOUT in " % stage % "\n"
		%> (msg	% "\nPlease report this bug to the maintainers at:\n"
			% Version.maintainers))
		a


-- | A compile time error in the user program. 
--	Report the errors and bail out. Using this version doesn't give the
--	compiler the chance catch the error and write it to a file, it just
--	compilains to stderr and dies. Using `exitWithUserError` is preferable
--	for a clean shutdown.
dieWithUserError 
	:: Pretty err PMode
	=> [err] 		-- ^ errors
	-> a

dieWithUserError  errs
	= error	(pprStrPlain $ "ERROR\n" % (punc "\n" errs))

	
-- | A compile time error in the user program.
--	If the args have StopErrors set, then write the errors to a file, 
--	otherwise write them to stderr. A clean exit. Using this is
--	preferable to using `dieWithUserError`.
exitWithUserError
	:: Pretty a PMode
	=> [Arg] 
	-> [a] 
	-> IO b
		
exitWithUserError args errs
 = case [e | e@StopErrors{} <- args] of
  	(StopErrors [file] : _)
	 -> do 	writeFile file 
			(pprStrPlain $ punc "\n" errs)
			
		exitWith ExitSuccess
		
	_ -> 	dieWithUserError errs


