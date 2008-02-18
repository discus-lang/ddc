-- Error handling.
--	Provides functions for emitting panics, freakouts and warnings.
--
module Shared.Error
	( panic
	, freakout
	, warning
	, dieWithUserError
	, exitWithUserError)
where

-----
import Util
import Debug.Trace
import Main.Arg
import System.Exit
import qualified Main.Version	as Version

-- | The walls are crashing down.
--	Print a last, dying message and bail out.
--
panic :: Pretty msg => String -> msg -> a
panic  stage msg
	=  error
		( pprStr $ "PANIC in " % stage % "\n" 
		%> (msg  % "\nPlease report this bug to the maintainers at:\n"
			 % Version.maintainers))


-- | Something bad has happened, and it's likely to be terminal.
--	Hopefully we can contine on for a bit longer until some other error occurs
--	that gives information about what caused this problem.
--
freakout :: Pretty msg => String -> msg -> a -> a
freakout stage msg a
	= trace 
		(pprStr $ "FREAKOUT in " % stage % "\n"
		%> (msg	% "\nPlease report this bug to the maintainers at:\n"
			% Version.maintainers))
		a

	
-- | Something troubling has happened, but it's not likely to be terminal.
--	We'll print the message to the console to let the user know that something's up.
--
warning :: Pretty msg => String -> msg -> a -> a
warning stage msg a
	= trace (pprStr $ "WARNING in " % stage % "\n" %> msg) 
		a


-- | A regular compile time error in the user program.
--	Report the errors and bail out.
--
dieWithUserError :: Pretty err => [err] -> a
dieWithUserError  errs
	= error	(pprStr $ "ERROR\n" ++ (catInt "\n" $ map pprStr errs))

	
-- | A compile time error in the user program
--	If the args have StopErrors set, then write the errors to a file, 
--	otherwise write them to stderr
--
exitWithUserError 
	:: Pretty a
	=> [Arg] -> [a] -> IO ()
		
exitWithUserError args []
	= return ()
		
exitWithUserError args errs
 = case filter (=@= StopErrors{}) args of
  	(StopErrors [file] : _)
	 -> do 	writeFile file 
			(catInt "\n" $ map pprStr errs)
			
		exitWith ExitSuccess
		
	_ -> 	dieWithUserError errs
