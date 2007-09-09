
module Shared.Error
(
	panic,
	death,
)

where

-----
import Util
import qualified Util.PrettyPrint as PP
import Shared.Pretty

-----
errorP x	= error x

-----
-- panic
--	Compiler panic.
--	Something quite unexpected happened.
--	Indicates a compiler bug.
--
panic  stage str
	= errorP 
	$  pretty
	$ "\nPANIC in " % stage	% "\n"
	%> str
	
	
-----
-- death
--	Kill the program and report these errors
--
death :: Pretty err => [err] -> a
death  errs
	= errorP ("ERROR\n\n" ++ (catInt "\n" $ map pretty errs))

	
