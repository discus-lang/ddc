
module DDC.Type.Data.Pretty
	( pprDataDefAsSource
	, pprCtorDefAsSource)
where
import DDC.Type.Pretty
import DDC.Type.Data.Base
import DDC.Type.Operators.Strip
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


-- | Pretty print a data type definition in source syntax.
pprDataDefAsSource :: DataDef -> Str
pprDataDefAsSource (DataDef vData vsData ctors)
	| Map.null ctors
	= "data " % vData % " " % (punc (ppr " ") (map ppr vsData)) 

	| otherwise
	= "data " % vData % " " % (punc (ppr " ") (map ppr vsData)) 
	%> ("\n= "
		% (punc (ppr "\n| ") 
			$ map pprCtorDefAsSource 
			$ Map.elems ctors))


-- | Pretty print a data constructor definition in the source syntax
pprCtorDefAsSource :: CtorDef -> Str
pprCtorDefAsSource ctorDef
	| ctorDefArity ctorDef == 0
	= ppr $ ctorDefName ctorDef

	| otherwise
	= (ctorDefName ctorDef)
		% "\n"
		%> ("{ " %
			( punc "\n; " 
			$ map pprField
			$ fieldTypeLabels ctorDef)
		% " }")
		
	where	pprField (Nothing, 	t)	
			= prettyTypeParens $ stripToBodyT t
			
		pprField (Just label,	t)	
			= label % " :: " 
			% (prettyTypeParens $ stripToBodyT t)


