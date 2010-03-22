
-- | Error handling. Provides functions for emitting panics, and freakouts.
module Shared.Error
	( panic
	, freakout
	, dieWithUserError
	, exitWithUserError)
where
import System.Exit
import Debug.Trace
import DDC.Main.Pretty
import DDC.Main.Arg	
import qualified DDC.Config.Version	as Version


-- | The walls are crashing down.
--	Print a last, dying message and bails out.
panic 	:: Pretty msg PMode
	=> String 		-- ^ name of module the panic was in.
	-> msg 			-- ^ message to display.
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
	=> String 		-- ^ name of module the freakout was in.
	-> msg 			-- ^ message to display.
	-> a 			-- ^ value to return
	-> a

freakout stage msg a
	= trace 
		(pprStrPlain
		$ "FREAKOUT in " % stage % "\n"
		%> (msg	% "\nPlease report this bug to the maintainers at:\n"
			% Version.maintainers))
		a


-- | A regular compile time error in the user program.
--	Report the errors and bail out.
dieWithUserError 
	:: Pretty err PMode
	=> [err] 		-- ^ errors
	-> a

dieWithUserError  errs
	= error	(pprStrPlain $ "ERROR\n" % (punc "\n" errs))

	
-- | A compile time error in the user program
--	If the args have StopErrors set, then write the errors to a file, 
--	otherwise write them to stderr.
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


