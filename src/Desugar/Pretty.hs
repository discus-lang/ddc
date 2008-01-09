{-# OPTIONS -fwarn-incomplete-patterns #-}

-- | Pretty printing for desugared source.
module Desugar.Pretty
	()

where

import Util
import Desugar.Exp
import Type.Pretty

-----
annot nn x
 = case nn of
 	Nothing	-> x
	Just n	-> "[" % n % ": " % x % "]"

-- Top -------------------------------------------------------------------------
instance Pretty a => Pretty (Top (Maybe a)) where
 prettyp xx
  = case xx of
  	PNil		-> prettyp "@PNil\n"

	PImport nn ms
	 -> annot nn
	 	("import {\n"
	 		%> ("\n" %!% (map (\m -> m % ";") ms))
			% "\n}\n\n")

	PExtern nn v tv to
	 -> annot nn 
	 	("extern " % v	
			%> ("\n" % ":: " % prettyTS tv	% "\n"
				 % ":$ " % to		% ";\n"))
		% "\n"

	PEffect nn v k
	 -> annot nn
	 	("effect " % v %>> " :: " % k) % ";\n"
		
	PRegion nn v
	 -> annot nn
	 	("region " % v) % ";\n"

	-- data defs
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
	 	("class " % v %>> " :: " % k) % ";\n"

	PClassDict nn v ts context sigs
	 -> annot nn
	 	("class " % v % " " % " " %!% map prettyTB ts % " where\n"
			% "{\n"
			%> ("\n\n" %!% map (\(v, sig) -> v % ("\n        :: " %> prettyTS sig 	% ";")) sigs)
			% "\n}\n")

	PClassInst nn v ts context defs
	 -> annot nn 
	 	("instance " % v % " " % " " %!% map prettyTB ts % " where\n"
			% "{\n"
			%> ("\n\n" %!% map (\(v, exp) -> v % ("\n =      " %> exp 		% ";")) defs) % "\n"
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
	 	(prettyp x) % ";\n\n"


-- CtorDef ---------------------------------------------------------------------
instance Pretty a => Pretty (CtorDef (Maybe a)) where
 prettyp xx
  = case xx of
  	CtorDef nn v []	-> prettyp v
	
	CtorDef nn v fs
	 -> annot nn
	 	(v % " {\n"
			%> ("\n" %!% fs) % "\n" % "}")


-- Exp -------------------------------------------------------------------------
instance Pretty a => Pretty (Exp (Maybe a)) where
 prettyp xx	
  = case xx of
  	XNil				-> prettyp "@XNil"
	XVoid		nn		-> annot nn (prettyp "@XVoid")
	XConst		nn c		-> annot nn (prettyp c)
	XVar		nn v		-> annot nn (prettyp v)
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

	XProjTagged 	nn v x j	-> annot nn ("@XProjTagged " % v % " " % prettyXB x % " " % j)

	XProjTaggedT 	nn v j		-> annot nn ("@XProjTaggedT " % v % " " % j)


	 

prettyXB xx
 = case xx of
	XLambda{}	-> "\n" %> ("(" % xx % ")")
	XVar{}		-> prettyp xx
	_		-> "(" % xx % ")"


-- Proj ------------------------------------------------------------------------
instance Pretty a => Pretty (Proj (Maybe a)) where
 prettyp xx
  = case xx of
  	JField  nn v		-> annot nn ("." % v)
	JFieldR nn v		-> annot nn ("#" % v)
	

-- Stmt ------------------------------------------------------------------------
instance Pretty a => Pretty (Stmt (Maybe a)) where
 prettyp xx
  = case xx of
	SBind nn Nothing x
	 -> annot nn
	 	(prettyp x) % ";"

  	SBind nn (Just v) x@XLambda{}		
	 -> annot nn 
	 	(v % "\n =      " %> x) % ";\n"
	
	SBind nn (Just v) x
	 -> annot nn
	 	(v %>> " = " % x)	% ";"

	

	SSig  nn v  t		-> annot nn (v  % " :: " % t) % ";"


-- Alt -------------------------------------------------------------------------
instance Pretty a => Pretty (Alt (Maybe a)) where
 prettyp xx
  = case xx of
	AAlt	 nn [] x	-> annot nn ("\\= " % x) 				% ";"
	AAlt	 nn gs x	-> annot nn ("|"  % "\n," %!% gs % "\n= " % x) 		% ";"


-- Guard -----------------------------------------------------------------------
instance Pretty a => Pretty (Guard (Maybe a)) where
 prettyp gg
  = case gg of
  	GCase nn pat		-> annot nn ("| " % pat)
	GExp  nn pat exp	-> annot nn (" "  % pat %>> " <- " % exp)
	

-- Pat ------------------------------------------------------------------------
instance Pretty a => Pretty (Pat (Maybe a)) where
 prettyp ww
  = case ww of
	WConLabel nn v lvs	
		-> annot nn (
			v % " {" % 
				", " %!% (map (\(l, v) -> l % " = " % v) lvs) % 
			"}")

	WConst	nn c		
		-> annot nn (prettyp c)


	WVar nn v
		-> annot nn (prettyp v)

	WAt nn v w
		-> annot nn (v % "@(" % w % ")")

	WConLabelP nn v lvs	
		-> annot nn (
			v % " (--pat--){" % 
				", " %!% (map (\(l, v) -> l % " = " % v) lvs) % 
			"}")

	WWildcard nn
		-> annot nn (prettyp "_")

-- Label -----------------------------------------------------------------------
instance Pretty a => Pretty (Label (Maybe a)) where
 prettyp ll
  = case ll of
  	LIndex	nn i		-> annot nn ("." % i)
	LVar	nn v		-> annot nn ("." % v)
	
