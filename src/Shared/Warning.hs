-- | Compiler Warnings.
module Shared.Warning
	( Warning (..))
where
import Shared.Var
import Util
import DDC.Main.Pretty
import Shared.VarUtil		(prettyPos)


-- | Abstract description of thw warning messages that we support.
data Warning
	= WarnRedundantOp Var
	| WarnUselessBinding Var
        | WarnUnknownPragma Var


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

