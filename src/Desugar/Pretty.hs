{-# OPTIONS -fwarn-incomplete-patterns #-}

-- | Pretty printing for desugared source.
module Desugar.Pretty
	(stripAnnot)
where
import Desugar.Exp
import Desugar.Plate.Trans
import Type.Pretty
import Type.Exp
import DDC.Main.Error
import DDC.Main.Pretty		
import qualified Shared.Var	as Var

stage = "Desugar.Pretty"

stripAnnot xx	= transformN (\n -> Nothing :: Maybe ()) xx

-----
annot nn x
 = case nn of
 	Nothing	-> x
	Just n	-> "[" % n % ": " % x % "]"

pprVar_unqual var
 = ppr $ var { Var.nameModuleId = Var.ModuleIdNil }

-- Top -------------------------------------------------------------------------
instance Pretty a PMode => Pretty (Top (Maybe a)) PMode where
 ppr xx
  = case xx of
  	PNil		-> ppr "@PNil\n"

	PImport nn ms
	 -> annot nn
	 	("import {\n"
	 		%> ("\n" %!% (map (\m -> m % ";") ms))
			% "\n}\n\n")

	PExtern nn v tv (Just to)
	 -> annot nn 
	 	("extern " % v	
			%> ("\n" % ":: " % prettyTS tv	% "\n"
				 % ":$ " % to		% ";\n"))
		% "\n"

	PExtern nn v tv Nothing
	 -> annot nn 
	 	("extern " % v	
			%> ("\n" % ":: " % prettyTS tv	% ";\n"))
		% "\n"

	PExternData nn s v k
	 -> annot nn
		("extern " % v
			%> ("\n" % "::" % k % ";\n"))

	PEffect nn v k
	 -> annot nn
	 	("effect " % v %> " :: " % k) % ";\n"
		
	PRegion nn v
	 -> annot nn
	 	("region " % v) % ";\n"

	-- types
	PTypeKind nn v k
	 -> annot nn 
	 	("type" <> v <> "::" <> k % ";\n\n")

	PTypeSynonym nn v t
	 -> annot nn 
		("type " <> v <> " = " % t % ";\n\n")

	PData nn v vs []
	 -> annot nn 
	 	("data " % " " %!% (v : vs)) % ";\n\n"

	PData nn v vs ctors
	 -> annot nn
	 	("data " % " " %!% (v : vs) % "\n"
		%> ("= "  % "\n\n| " %!% ctors % ";")
		% "\n\n")
		
	-- classes
	PClass nn v k
	 -> annot nn
	 	("class " % v %> " :: " % k) % ";\n"

	PClassDict nn v ts context sigs
	 -> annot nn
	 	("class " % pprVar_unqual v % " " % " " %!% map pprPClassDict_varKind ts % " where\n"
			% "{\n"
			%> ("\n\n" %!% map (\(v', sig) -> pprVar_unqual v' % ("\n        :: " %> prettyTS sig 	% ";")) sigs)
			% "\n}\n")

	PClassInst nn v ts context ss
	 -> annot nn 
	 	("instance " % v % " " % " " %!% map prettyTB ts % " where\n"
			% "{\n"
			%> ("\n\n" %!% ss) % "\n"
			% "}\n\n")

	-- projections
	PProjDict nn t ss
	 -> annot nn
	 	("project " % t % " where\n"
			% "{\n"
			%> ("\n\n" %!% ss) % "\n"
			% "}\n\n")



	PSig nn v t
	 -> annot nn
	 	(v %> ("\n:: " % prettyTS t))	% ";\n\n"

	PBind nn (Just v) x
	 -> annot nn 
	 	(v % "\n =      " %> x) % ";\n\n"

	PBind nn Nothing x
	 -> annot nn
	 	(ppr x) % ";\n\n"

pprPClassDict_varKind tt
 = case tt of
	TVar k v	-> parens $ v <> "::" <> k
	_		-> panic stage "pprPClassDict_varKind: no match\n"

-- CtorDef ---------------------------------------------------------------------
instance Pretty a PMode => Pretty (CtorDef (Maybe a)) PMode where
 ppr xx
  = case xx of
  	CtorDef nn v []	-> ppr v
	
	CtorDef nn v fs
	 -> annot nn
	 	(v % " {\n" %> ("\n" %!% fs) % "\n" % "}")


-- Exp -------------------------------------------------------------------------
instance Pretty a PMode => Pretty (Exp (Maybe a)) PMode where
 ppr xx	
  = case xx of
  	XNil				-> ppr "@XNil"
	XVoid		nn		-> annot nn (ppr "@XVoid")
	XLit		nn l		-> annot nn (ppr l)
	XVar		nn v		-> annot nn (ppr v)
	XVarInst 	nn v		-> annot nn ("@XVarInst " % v)
	XProj 		nn x j		-> annot nn ("@XProj "  % prettyXB x % " " % j)
	XProjT		nn t j		-> annot nn ("@XProjT " % prettyTB t % " " % j)
	XLambda    	nn v x 		-> annot nn ("\\" % v  % " ->\n" % x)
	XLambdaTEC 	nn v x t eff clo -> annot nn ("\\" % v  % " (" % eff % " " % clo % ") :: " % t % " ->\n" % x)


	XApp	Nothing x1 x2	
	 -> x1 % " " % prettyXB x2

	XApp	nn x1 x2	
	 -> annot nn (x1 %> ("\n" % prettyXB x2))

	XMatch  nn Nothing aa	
	 -> annot nn 
	 	("match {\n"
		 %> "\n\n" %!% aa
		 % "\n}")


	XMatch  nn (Just x1) aa	
	 -> annot nn 
	 	("match " % x1 % " with {\n"
		 %> "\n\n" %!% aa
		 % "\n}")
	 
	XDo	nn ss
	 -> annot nn 
	 	("do {\n"
		 %> ("\n" %!% ss)
		 % "\n}")
	 
	XIfThenElse nn x1 x2 x3
	 -> annot nn 
	 	("if " % x1 % "\n" %> (" then " % x2) % "\n" %> (" else " % x3))

	XProjTagged 	nn vI tC x j	-> annot nn ("@XProjTagged " % vI % " " % tC % " " % prettyXB x % " " % j)

	XProjTaggedT 	nn vI tC j	-> annot nn ("@XProjTaggedT " % vI % " " % tC % j)


	 

prettyXB xx
 = case xx of
	XLambda{}	-> "\n" %> ("(" % xx % ")")
	XVar{}		-> ppr xx
	_		-> "(" % xx % ")"


-- Proj ------------------------------------------------------------------------
instance Pretty a PMode => Pretty (Proj (Maybe a)) PMode where
 ppr xx
  = case xx of
  	JField  nn v		-> annot nn ("." % v)
	JFieldR nn v		-> annot nn ("#" % v)
	

-- Stmt ------------------------------------------------------------------------
instance Pretty a PMode => Pretty (Stmt (Maybe a)) PMode where
 ppr xx
  = case xx of
	SBind nn Nothing x
	 -> annot nn
	 	(ppr x) % ";"

  	SBind nn (Just v) x@XLambda{}		
	 -> let v'	= v { Var.nameModuleId = Var.ModuleIdNil}
	    in  annot nn 
	  	 	(v' %>> "\n =      " %> x) % ";\n"
	 
	SBind nn (Just v) x
	 -> let v'	= v { Var.nameModuleId = Var.ModuleIdNil}
	    in  annot nn
	 	 	(v' %>> " = " % x)	% ";"

	SBindMonadic nn pat x
	 -> annot nn
	    		(pat %>> " <- " % x)	% ";"
	
	SBindPat nn pat x
	 -> annot nn	(pat %>> " = " % x)	% ";"
	
	SSig  nn vs  t	
	 -> let vs'	= map (\v -> v { Var.nameModuleId = Var.ModuleIdNil}) vs
	    in	annot nn (vs' %!% ", " %>> " :: " % t) % ";"


-- Alt -------------------------------------------------------------------------
instance Pretty a PMode => Pretty (Alt (Maybe a)) PMode where
 ppr xx
  = case xx of
	AAlt	 nn [] x	-> annot nn ("\\= " % x) 				% ";"
	AAlt	 nn gs x	-> annot nn ("|"  % "\n," %!% gs % "\n= " % x) 		% ";"


-- Guard -----------------------------------------------------------------------
instance Pretty a PMode => Pretty (Guard (Maybe a)) PMode where
 ppr gg
  = case gg of
  	GCase nn pat	-> annot nn ("| " % pat)
	GExp  nn pat x	-> annot nn (" "  % pat %> " <- " % x)
	

-- Pat ------------------------------------------------------------------------
instance Pretty a PMode => Pretty (Pat (Maybe a)) PMode where
 ppr ww
  = case ww of
	WConLabel nn v lvs	
		-> annot nn (
			v % " {" % 
				", " %!% (map (\(l, v') -> l % " = " % v') lvs) % 
			"}")

	WLit	nn l
		-> annot nn (ppr l)


	WVar nn v
		-> annot nn (ppr v)

	WAt nn v w
		-> annot nn (v % "@(" % w % ")")

	WConLabelP nn v lvs	
		-> annot nn (
			v % " (--pat--){" % 
				", " %!% (map (\(l, v') -> l % " = " % v') lvs) % 
			"}")

-- Label -----------------------------------------------------------------------
instance Pretty a PMode => Pretty (Label (Maybe a)) PMode where
 ppr ll
  = case ll of
  	LIndex	nn i		-> annot nn ("." % i)
	LVar	nn v		-> annot nn ("." % v)
	
