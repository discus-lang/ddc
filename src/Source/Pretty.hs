{-# OPTIONS -fwarn-incomplete-patterns #-}

-----
-- Source.Pretty
--
-- Summary:
--	Pretty printer for Source.Exp expressions.
--	Must keep output format parseable by trauma parser.
--
--
module Source.Pretty
(
	pretty,
)

where

-----
import Util

-----
import Util.Pretty
import qualified Util.PrettyPrint as PP

-----
import qualified Shared.Var 	as Var
import Shared.Error

import Source.Exp
import Source.Horror
import Source.Util
import Shared.Pretty

import Type.Pretty

-----
stage	= "Source.Pretty"

-----
-- prettyTop
--
instance Pretty Top where
 prettyp xx
  = case xx of
	PPragma es	 -> "pragma " % " " %!% es % ";\n"
	PModule v	 -> "module " % v % ";\n"

	PImportExtern  v tv to
	 -> "import extern " % prettyVTT v tv to % "\n"

	PImportModule [m] -> "import " % m % ";\n"
	
	PImportModule xx
	 -> "import " 
		% "{\n"
			%> "\n" %!% map (\x -> x % ";") xx
		% "\n}\n\n"
		
	PForeign f 
	 -> "foreign " % f % ";\n\n";
	
	PData typeName vars []
	 -> "data " % " " %!% (typeName : vars) % ";\n\n"

	PData typeName vars ctors
	 -> "data " % " " %!% (typeName : vars) % "\n"
		%> ("= "  % "\n\n| " %!% (map prettyCtor ctors) % ";")
		%  "\n\n"

	PRegion v	 -> "region " % v % ";\n"
	PEffect v k	 -> "effect " % v %>> " :: " % k % ";\n"

	-- Classes
	PClass v k	 -> "class " % v %>> " :: " % k % ";\n"

	PClassDict c vs inh sigs
	 -> "class " % c % " " % (" " %!% vs) % " where\n"
		% "{\n"
	 	%> ("\n\n" %!% (map (\(vs, t) -> ", " %!% vs % " :: " % t % ";") sigs)) % "\n"
		% "}\n\n"

	PClassInst v ts inh ss
	 -> "instance " % v % " " % " " %!% (map prettyTB ts) % " where\n"
		% "{\n"
		%> ("\n\n" %!% ss) % "\n"
		% "}\n\n"

	-- Projections
	PProjDict t ss
	 -> "project " % t % " where\n"
		% "{\n"
		%> ("\n\n" %!% ss) % "\n"
		% "}\n\n"
	 

	-- type sigs	 
	PType sp v t
	 -> let (op, x)
	 	 = case t of
			TSig (TQuant x)		-> ("::", x)
			TSig x			-> (":*", x)
			
			TSigExact (TQuant x)	-> (":::", x)
			TSigExact x		-> ("::*", x)
			_		-> panic stage $ "pretty[Top]: no match for " % show xx

	    in	 v %>> op % prettyTS x % ";\n"
	 

	PStmt s		 -> prettyp s % "\n\n"
	
	PInfix mode prec syms
	 -> mode % " " % prec % " " % ", " %!% (map Var.name syms) % " ;\n"


prettyVTT ::	Var -> 	Type -> Maybe Type	-> PrettyP
prettyVTT   	v	tv	mto
	=  v 	% "\n"
			%> (":: " % prettyTS tv % "\n" 
			% case mto of 
				Nothing	-> prettyp ""
				Just to	-> ":$ " % to % "\n")


prettyCtor ::	(Var, [DataField Exp Type])	-> PrettyP
prettyCtor	xx
 = case xx of
 	(v, [])		-> prettyp v
	(v, fs)		
	 -> v % " {\n"
		%> ("\n" %!% (map pretty fs)) % "\n"
		% "}"
-----
-- Foreign
--
instance Pretty Foreign where
 prettyp ff
  = case ff of
  	OImport f		-> "import " % f
	OExport f		-> "export " % f
	
	OExtern mS v tv mTo
	 -> let pName	= case mS of  { Nothing -> prettyp " "; Just s  -> prettyp $ show s }
		pTo	= case mTo of { Nothing -> prettyp " "; Just to -> "\n:$ " % to }
	    in 
	 	"extern " % pName % "\n " 
		 % v 	%> ("\n:: " % prettyTS tv 	% pTo)

	OCCall mS v tv 
	 -> prettyp "@CCall"

-----
-- InfixMode
--
instance Pretty InfixMode where
 pretty mode
  = case mode of
 	InfixLeft	-> "infixl"
	InfixRight	-> "infixr"
	InfixNone	-> "infix "
	InfixSuspend	-> "@InfixSuspend"
	
-----
-- Exp
--
instance Pretty Exp where
 prettyp xx
  = case xx of
  	XNil		 -> prettyp "@XNil"
	XAnnot aa e	 -> aa % prettyXB e

	XUnit sp	 -> prettyp "()"

	XVoid	sp	 -> prettyp "_"
	XConst 	sp c	 -> prettyp c

	XVar 	sp v	 -> prettyp v

	XProj 	sp x p	 -> prettyXB x % p

	XLambda sp v e	 -> "\\" % v % " -> " % e


	XLet 	sp ss e
	 -> "let {\n" 
		%> ";\n" %!% ss
		%  "\n} in " % e

	XDo 	sp ss	 -> "do {\n" %> "\n" %!% ss % "\n}"

	XCase 	sp co ca
	 -> "case " % co % " of {\n" 
	 	%> "\n\n" %!% ca
		%  "\n}"

	XLambdaPats sp ps e
	 -> "\\" % " " %!% ps % " -> " % e

	XLambdaProj sp j xs
	 -> "\\" % j % " " % xs

	XLambdaCase sp cs
	 -> "\\case {\n"
	 	%> "\n\n" %!% cs
		%  "\n}"

	XCaseE 	sp co ca eff
	 -> "case " % co % " of " % eff % " {\n" 
	 	%> "\n\n" %!% ca
		%  "\n}"

	XApp 	sp e1 e2
	 -> if orf e1 [isXVar, isXApp, isXAnnot, isXUnit]

	 	then e1 % " " % prettyXB e2

		else "(" % e1 % ") " %> prettyXB e2
		
 
	XIfThenElse sp e1 e2 e3
	 ->  "if " % (if isEBlock e1 then "\n" else " ") % e1 
		%> ("\nthen " 	% (if isEBlock e2 then "\n" else " ") % e2)

		% (if isEBlock e2 then "\n" else " ")
		%> ("\nelse "	% (if isEBlock e3 then "\n" else " ") % e3)

	-----
	XAt 	sp v exp	 -> v % "@" % prettyXB exp

	-- object expressions
	XObjVar 	sp v	 -> "^" % v
	XObjField 	sp v	 -> "_" % v

	-- infix expressions
	XOp 		sp v	 -> "@XOp " % v
	XDefix 		sp es	 -> "@XDefix " % es
 	XDefixApps 	sp es	 -> "@XDefixApps " % es

	-- lambda sugar


	-- match sugar
	XMatch sp aa	
	 -> "match {\n" 
	 	%> "\n\n" %!% aa
		%  "\n}"
	
	-- exception sugar
	XTry sp x aa Nothing
	 -> "try " % prettyXB x % "\n"
	 %  "catch {\n" 
	 %> ("\n" %!% aa)
	 %  "};"

	XTry sp x aa (Just wX)
	 -> "try " % prettyXB x % "\n"
	 %  "catch {\n" 
	 %> ("\n" %!% aa)
	 %  "}\n"
	 %  "with " % wX % ";"

	-- imperative sugar
	XWhile sp x1 x2
	 -> "while (" % x1 % ")\n"
	 % x2
	 
	XWhen sp x1 x2
	 -> "when (" % x1 % ")\n"
	 % x2
	 
	XUnless sp x1 x2
	 -> "unless (" % x1 % ")\n"
	 % x2
	
	XBreak sp 
	 -> prettyp "break"
	 
	-- list range sugar
	XListRange sp b x Nothing		-> "[" % x % "..]"
	XListRange sp b x (Just x2)	-> "[" % x % ".." % x2 % "]"
	
	
	XListComp sp x qs 		-> "[" % x % " | " % ", " %!% qs % "]"
	

	-- patterns
	XCon   sp v xx			-> v % " " % " " %!% xx
	XTuple sp xx			-> "(" % ", " %!% xx % ")"
	XCons  sp x1 x2			-> x1 % ":" % x2
	XList  sp xx			-> "[" % ", " %!% xx % "]"

	_ 	-> panic stage
		$  "pretty[Exp]: not match for " % show xx

instance Pretty Proj where
 prettyp f
  = case f of
  	JField  l	-> "." % l
	JFieldR l	-> "#" % l

	JIndex	x	-> ".[" % x % "]"
	JIndexR	x	-> "#[" % x % "]"

	JAttr	v	-> ".{" % v % "}"

-----
isEBlock x
	= orf x	[ isXLet 
		, isXCase
		, isXDo ]
	
prettyXB xx
 = case xx of
 	XVar sp v	-> prettyp v
	XConst sp c	-> prettyp c
	XUnit sp	-> prettyp xx
	XProj{}		-> prettyp xx
	e		-> "(" % e % ")"
	
prettyX_naked xx
 = case xx of
	XApp sp e1 e2	-> prettyX_appL (XApp sp e1 e2)
	e		-> prettyp e


prettyX_appL xx
 = case xx of
	XApp sp e1 e2
	 -> prettyX_appL e1 
		%  " "
		% prettyX_appR e2

	e  -> prettyXB e


-----
prettyX_appR xx
 = case xx of
	XApp sp e1 e2	->  "(" % prettyX_appL e1 % " " % prettyX_appR e2 % ")"
	e 		-> prettyXB e

-----
instance Pretty Annot where
 prettyp xx 
  = case xx of
  	ATypeVar   v	-> "@T " % v
	AEffectVar v	-> "@E " % v

-----
instance Pretty Alt where
 prettyp a
  = case a of
	APat 	 p1 x2		-> p1	% "\n -> " % prettyX_naked x2 % ";"

	AAlt	 [] x		-> "\\= " % x % ";"
	AAlt	 gs x		-> "|"  % "\n," %!% gs % "\n=  " % x % ";"

	ADefault x		-> "_ ->" % x % "\n"
	

instance Pretty Guard where
 prettyp gg
  = case gg of
  	GCase pat		-> "- " % pat
	GExp  pat exp		-> " "  % pat %>> " <- " % exp
	GBool exp		-> " "  % prettyp exp
	GBoolU exp		-> "# " % exp
	
instance Pretty Pat where
 prettyp ww
  = case ww of
  	WVar v			-> prettyp v
	WConst c		-> prettyp c
	WCon v ps		-> v % " " % ps %!% " " 
	WConLabel v lvs		-> v % " { " % ", " %!% map (\(l, v) -> l % " = " % v ) lvs % "}"
	WAt v w			-> v % "@" % w
	WWildcard 		-> prettyp "_"
	WExp x			-> prettyp x

instance Pretty Label where
 prettyp ll
  = case ll of
  	LIndex i		-> "." % i
	LVar   v		-> "." % v

-----
instance Pretty LCQual where
 prettyp q
  = case q of
  	LCGen False p x	-> p % " <- " % x
	LCGen True  p x -> p % " <@- " % x
	LCExp x		-> prettyp x
	LCLet ss	-> "let { " % ss % "}"

-----
instance Pretty Stmt where
 prettyp xx
  = case xx of
	SBind sp Nothing x	-> prettyX_naked x 					% ";"
	SBind sp (Just v) x	-> v 			%>> (spaceDown x) % " = " % prettyX_naked x 	% ";"

	SBindPats sp v [] x	-> v % " " 		%>> (spaceDown x) % " = " % prettyX_naked x 	% ";"
	SBindPats sp v ps x	-> v % " " % " " %!% ps %>> (spaceDown x) % " = " % prettyX_naked x 	% ";"


	SSig sp v t		-> v %> " :: " % t % ";"

spaceDown xx
 = case xx of
	XLambda{}	-> prettyp "\n"
	XLambdaCase{}	-> prettyp "\n"
 	XCase{}		-> prettyp "\n"
	XCaseE{}	-> prettyp "\n"
	XIfThenElse{}	-> prettyp "\n"
	XDo{}		-> prettyp "\n"
	_		-> pNil


-- instance Pretty FixDef where
--  pretty	= prettyFD

prettyFD :: 	FixDef -> String
prettyFD 	(v, (fixity, mode))
	= pretty mode ++ " " ++ show fixity ++ " " ++ Var.name v ++ " ;"
	


