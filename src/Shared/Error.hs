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
import Main.Arg
import qualified Main.Version	as Version

import Shared.Pretty
import System.Exit
import Debug.Trace
import Util

-- | The walls are crashing down.
--	Print a last, dying message and bail out.
--
panic 	:: Pretty msg PMode
	=> String -> msg -> a
panic  stage msg
	=  error
		( pprStrPlain
		$ "PANIC in " % stage % "\n" 
		%> (msg  % "\nPlease report this bug to the maintainers at:\n"
			 % Version.maintainers))


-- | Something bad has happened, and it's likely to be terminal.
--	Hopefully we can contine on for a bit longer until some other error occurs
--	that gives information about what caused this problem.
--
freakout 
	:: Pretty msg PMode
	=> String -> msg -> a -> a

freakout stage msg a
	= trace 
		(pprStrPlain
		$ "FREAKOUT in " % stage % "\n"
		%> (msg	% "\nPlease report this bug to the maintainers at:\n"
			% Version.maintainers))
		a

	
-- | Something troubling has happened, but it's not likely to be terminal.
--	We'll print the message to the console to let the user know that something's up.
--
warning 
	:: Pretty msg PMode
	=> String -> msg -> a -> a

warning stage msg a
	= trace	( pprStrPlain
		$ "WARNING in " % stage % "\n" %> msg) 
		a


-- | A regular compile time error in the user program.
--	Report the errors and bail out.
--
dieWithUserError 
	:: Pretty err PMode
	=> [err] -> a

dieWithUserError  errs
	= error	(pprStrPlain $ "ERROR\n" % (punc "\n" errs))

	
-- | A compile time error in the user program
--	If the args have StopErrors set, then write the errors to a file, 
--	otherwise write them to stderr
--
exitWithUserError 
	:: Pretty a PMode
	=> [Arg] -> [a] -> IO ()
		
exitWithUserError args []
	= return ()
		
exitWithUserError args errs
 = case filter (=@= StopErrors{}) args of
  	(StopErrors [file] : _)
	 -> do 	writeFile file 
			(pprStrPlain $ punc "\n" errs)
			
		exitWith ExitSuccess
		
	_ -> 	dieWithUserError errs
