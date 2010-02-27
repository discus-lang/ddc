-- Error handling.
--	Provides functions for emitting panics, freakouts and warnings.
--
module Shared.Warning
	( Warning (..)
    , warning)
where

-----
import Shared.Var
import Shared.VarUtil		(prettyPos)

import Shared.Pretty
import Debug.Trace
import Util


data Warning
	= WarnRedundantOp Var
	| WarnUselessBinding Var
        | WarnUnknownPragma Var


-- | Something troubling has happened, but it's not likely to be terminal.
--	We'll print the message to the console to let the user know that something's up.
--
warning :: Pretty Warning PMode
	=> Warning -> a -> a

warning warn a
	= trace	(pprStrPlain $ "Warning at " % warn) a

-- Pretty Printer ---------------------------------------------------------------------------------
-- | Pretty printer for warning messages
instance Pretty Warning PMode where

 ppr (WarnRedundantOp op)
 	= prettyPos op						% "\n"
	% "     Redundant operator '" % name op  % "' in expression.\n"

 ppr (WarnUselessBinding vBind)
	= prettyPos vBind					% "\n"
	% "     Binding '" % name vBind % "' has no uses.\n"

 ppr (WarnUnknownPragma p)
	= prettyPos p						% "\n"
	% "     Unknown pragma '" % name p % "\n"

