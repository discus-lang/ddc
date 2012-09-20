{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Top-level error handing functions.
--	The data types that represent the errors should be defined
--	closer to the modules that detect them, and not here.
module DDC.Main.Error
	( panic
	, freakout
	, warning
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
panic 	:: forall msg a
	.  Pretty msg PMode
	=> String 		-- ^ Name of the DDC module the panic was in.
	-> msg 			-- ^ Message to display.
	-> a 
	
panic  stage msg
	= error
	$ pprStrPlain
	$ "PANIC in " % stage % "\n" 
	%> (msg  % "\nPlease report this bug to the maintainers at:\n"
		 % Version.maintainers)


-- | Something bad has happened, and it's likely to be terminal.
--	Hopefully we can contine on for a bit longer until some other error occurs
--	that gives information about what caused this problem.
freakout 
	:: forall msg a
	.  Pretty msg PMode
	=> String 		-- ^ Name of the DDC module the freakout was in.
	-> msg 			-- ^ Message to display.
	-> a 			-- ^ Value to return.
	-> a

freakout stage msg a
	= trace 
		(pprStrPlain
		$ "FREAKOUT in " % stage % "\n"
		%> (msg	% "\nPlease report this bug to the maintainers at:\n"
			% Version.maintainers))
		a


-- | Something troubling has happened, but it's not likely to be terminal.
--	We'll print the message to the console to let the user know that
--	something's up.
warning :: forall msg a
	.  Pretty msg PMode
	=> msg 			-- ^ Warning to display.
	-> a 			-- ^ Value to return.
	-> a

warning warn a
	= trace	(pprStrPlain $ "ddc: WARNING\n" % warn) a


-- | Compilain about a compile time error in the user program, then bail out.
--	Using this version doesn't give the compiler the chance to honor
--	`StopErrors` by catching the error message and writing it to a file.
--	It just complains to stderr and dies. 
--	Using `exitWithUserError` is preferable for a clean shutdown.
dieWithUserError 
	:: forall err a
	.  Pretty err PMode
	=> [err] 		-- ^ Errors to display.
	-> a

dieWithUserError  errs
	= error	$ pprStrPlain $ "ERROR" % nl % vsep errs % nl

	
-- | Handle a compile time error in the user program in an orderly way.
--	If the args have StopErrors set, then write the errors to a file, 
--	otherwise write them to stderr. A clean exit. Using this is
--	preferable to using `dieWithUserError`.
exitWithUserError
	:: forall err a
	.  Pretty err PMode
	=> [Arg] 		-- ^ Command line args, possibly including `StopErrors`.
	-> [err] 		-- ^ Errors to display.
	-> IO a
		
exitWithUserError args errs
 = case [e | e@StopErrors{} <- args] of
  	(StopErrors [file] : _)
	 -> do 	writeFile file 
		 $ pprStrPlain (vsep errs % nl)
			
		exitWith ExitSuccess
		
	_ -> 	dieWithUserError errs


