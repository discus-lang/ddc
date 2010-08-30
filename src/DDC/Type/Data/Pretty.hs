
module DDC.Type.Data.Pretty
where
import DDC.Type.Pretty
import DDC.Type.Data.Base
import DDC.Main.Pretty
import qualified Data.Map	as Map
	

instance Pretty DataDef PMode where
 ppr (DataDef v vs ctors)
	| Map.null ctors
	= "data " % " " %!% (v : vs) % ";\n\n"

	| otherwise
	= "data " % " " %!% (v : vs) % "\n"
	%> ("= "  % "\n\n| " %!% (Map.elems ctors))


instance Pretty CtorDef PMode where
 ppr (CtorDef v t arity tag fs)
  = v 	% "\n"
	%> 	( ":: " % prettyTypeSplit t % "\n"
		% "with { ARITY  = " % arity	% "\n"
		% "     , TAG    = " % tag      % "\n"
		% "     , FIELDS = " % fs 	% "\n"
		% "}")
