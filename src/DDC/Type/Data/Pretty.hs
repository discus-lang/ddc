{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

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
 ppr def@(DataDef 
	{ dataDefName 	= v 
	, dataDefParams	= vks
	, dataDefCtors	= ctors })

	| Map.null ctors
	= "data " 
		% punc " " (v : map fst vks) 
		% " where\n" 
	%> vcat
		[ "KIND       = " % dataDefKind def
		, "SEANAME    = " % dataDefSeaName def
		, "MATERIAL   = " % dataDefMaterialVars def
		, "IMMATERIAL = " % dataDefImmaterialVars def
		, blank]

	| otherwise
	= "data " 
		% punc " " (v : map fst vks) 
		% " where\n" 
	%> vcat
		[ "KIND       = " % dataDefKind def
		, "MATERIAL   = " % dataDefMaterialVars def
		, "IMMATERIAL = " % dataDefImmaterialVars def
	   	, punc "\n" (Map.elems ctors)
		, blank]

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
pprDataDefAsSource 
 def@(DataDef 
	{ dataDefName	= vData
	, dataDefParams	= vksParam
	, dataDefCtors	= ctors })

	| Just name	<- dataDefSeaName def
	= "foreign import data " 
		% show name
		% " " % vData
		% " :: " 
		% dataDefKind def
	
	| Map.null ctors
	= "data " 
		% vData 
		% " " % (punc (ppr " ") (map ppr $ map fst vksParam)) 

	| otherwise
	= "data " 
		% vData 
		% " " % (punc (ppr " ") (map ppr $ map fst vksParam)) 
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


