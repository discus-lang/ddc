-- | Compiler Warnings.
module Shared.Warning
	( Warning (..))
where
import Util
import DDC.Main.Pretty
import Shared.VarUtil		(prettyPos)
import DDC.Var


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
	% "     Redundant operator '" % varName op  % "' in expression.\n"

 ppr (WarnUselessBinding vBind)
	= prettyPos vBind					% "\n"
	% "     Binding '" % varName vBind % "' has no uses.\n"

 ppr (WarnUnknownPragma p)
	= prettyPos p						% "\n"
	% "     Unknown pragma '" % varName p % "\n"

