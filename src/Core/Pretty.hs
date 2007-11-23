{-# OPTIONS -fwarn-incomplete-patterns #-}

module Core.Pretty
	( pretty )

where

import Core.Exp
import Core.Util.Bits
import Core.Util.Strip

import qualified Shared.Var	as Var
import Shared.Var (Var)
import Shared.Error

import qualified Data.Set	as Set

import Util
import Util.Pretty

-----
stage	= "Core.Pretty"

-----------------------
sv v	= padR 8 (pretty $ pv v)

-- force display of type namespace qualifier
pv v
 = case Var.nameSpace v of
 	Var.NameType	-> "*" % v
	_		-> prettyp v

-----------------------
-- prettyP
--
instance Pretty Top where
 prettyp xx
  = case xx of
	PNil
	 -> prettyp "@PNil"

	PBind v e
	 -> v % "\n"
		% " =      " %> e  % ";\n"

	PExtern v (TBot KData) tv
	 -> "extern " % v % ";\n"
	
	PExtern v tv to
	 -> "extern " % v % "\n"
	 %  " =      "
	 	%> (pretty tv % "\n"
		%  ":$ " % to % ";\n")

	PData v vs []
	 -> "data  " % " " %!% (v:vs) % ";\n"

	PData v vs cs
	 -> "data  " % " " %!% (v:vs) % "\n"  
	 	%> ("= " % "\n\n| " %!% cs % ";\n")

	PCtor v tv to
	 -> "@PCtor  " % v % "\n"
	 %  " =      "   
	 	%> (tv % "\n")
		%> (":$ " % to % ";\n")

	PRegion v 
	 -> "region " % v % ";\n"
	
	PEffect v k
	 -> "effect " % v % " :: " % k % ";\n"

	PClass v k
	 -> "class " % v % ";\n"


	PClassDict v ts context sigs
	 -> ("class " % v % " " % " " %!% map prettyTB ts % " where\n"
	 	% "{\n"
		%> (";\n\n" %!% map (\(v, sig) -> v % "\n ::     " %> sig) sigs)
		% "\n}\n")

	PClassInst v ts context defs
	 -> ("instance " % v % " " % " " %!% map prettyTB ts % "\n"
			% "{\n"
			%> ("\n\n" %!% (map (\(v, exp) 
						-> v % "\n" 
						% " =      " %> exp % ";") defs))
			% "\n"
			% "}\n\n")



-----------------------
-- CtorDef
--
instance Pretty CtorDef where
 prettyp xx
  = case xx of
  	CtorDef v fs
 	 -> v 	% "\n{\n"
	 	%> ("\n" %!% fs) % "\n}"
	 
	 
-----------------------
-- prettyX
--
instance Pretty Exp where
 prettyp xx
  = case xx of
	XNil	
	 -> prettyp "@XNil"

	XNothing
	 -> prettyp "@XNothing"

	XAnnot a e
	 -> "[" % a % ";\n " % e % "]"

	XVar v
	 -> pv v
		
	XLAM v k e
	 -> let -- split off vars with simple kinds
	 	takeLAMs acc exp@(XLAM v k x)
	 	  | elem k [TKind KRegion, TKind KEffect, TKind KClosure, TKind KData]	
		  		= takeLAMs (v : acc) x
	 
	 	takeLAMs acc exp
		 = (exp, acc)
	 
	        (xRest, vsSimple)	= takeLAMs [] xx
	    
	    in	case vsSimple of
	    	 []	-> "/\\ (" % padR 16 (sv v) % " :: " % k % ") ->\n" % e
		 _	-> "/\\  " % ", " %!% map pv (reverse vsSimple) % " ->\n" % xRest


	XLam v t x eff clo
	 -> "\\  (" % padR 16 (sv v) % " :: " % t % ")"
		 % pEffClo % " ->\n"
		 % x
	 
	 where	pEffClo	= case (eff, clo) of 
	 			(TBot KEffect, TBot KClosure) 	-> pNil
				_ -> "\n" % replicate 20 ' ' % " of " % eff % " " % clo
					 
	 
	XAPP x t
	 | spaceApp t
	 ->  x % "\n" 
	 	%> prettyTB t

	 | otherwise
	 ->  x % " " % prettyTB t


	XApp x1 x2 (TBot KEffect)
	 ->  x1 % " " % prettyExpB x2


	XApp e1 e2 eff
	 |  e1 =@= XVar{} || isXApp e1
	 ->        e1 % "\n" 
			%> (prettyExpB e2 % " " % prettyE_caused eff)
	 | otherwise
	 -> "(" % e1 % ")\n" 
		%> (prettyExpB e2 % " " % prettyE_caused eff)

	XTau t x
	 -> "[** " % prettyTB t % " ]\n" % x

	XTet vts x
	 -> "\n" %!%
	    (map (\(v, t) -> "let " % padR 16 (sv v) % " =  " % t % " in") vts)
	    % "\n" % x	 
	
	XDo bs
	 -> "do {\n"
	 	%> ";\n\n" %!% bs % ";\n}"

	XMatch alts _
	 -> "match {\n"
		%> ("\n\n" %!% alts)
		% "\n"
		% "}"

	XConst c t
	 -> "(" % c % " :: " % t % ")"

	XLocal v vts x
	 -> "local " % v %>> "  where {" % "; " 
	 	%!% (map (\(v, t) -> pv v % " = " % t) vts)
		% "} in\n" % x
	 

	-- prim
	XPrim m args (TBot KEffect)
	 -> m % " " % " " %!% map prettyExpB args

	XPrim m args eff
	 -> m % " " % " " %!% map prettyExpB args % "\n" %> prettyE_caused eff
	 

	XAtom v args
	 -> "@XAtom "	% v % " " % args 


	-- intermediate
	XAppF xs
	 -> "@XAppF " % xs
	
	XAppFP x eff
	 -> "@XAppFP " % x % " " % eff

	XAt v x
	 -> "@XAt " % v % " " % x

	XType t
	 -> prettyp t

	XLifted vLifted vsFree
	 -> "@XLifted " % vLifted % " " % vsFree



spaceApp xx
 = case xx of
	TVar{}			-> False
	_			-> True


prettyE_caused	eff
 = case eff of
	TBot KEffect	-> prettyp "<>"
	_		-> "<" % prettyp eff % ">"

prettyClosureV (v, (eff, env))
	=  v %  " = " 
		% prettyClosureEff eff 
		% prettyClosureEnv env
	

prettyClosureEff es
 = case es of
 	[]	-> prettyp " "
	_	-> " {" % ", " %!% es % "}"
	
prettyClosureEnv es
 = case es of
 	[]	-> prettyp " "
	_	-> " [" % ", " %!% es % "]"


prettyVK	(var, kind)
 = case kind of
 	KData		-> prettyp var
	KRegion		-> prettyp var
	KEffect		-> prettyp var
	KClosure	-> prettyp var
	_		-> "(" % var % " :: " % kind % ")"
	

prettyExpB x
 = case x of
	XVar{}		-> prettyp x
	XConst{}	-> prettyp x
	XAnnot{}	-> prettyp x
	XType t		-> prettyTB t
	_		-> "(" % x % ")"


-----------------------
-- Prim
--
instance Pretty Prim where
 prettyp xx 
  = case xx of
  	MSuspend v	-> "prim{Suspend} " 	% v
	MForce 		-> prettyp "prim{Force} "
	MBox	t1 t2	-> "prim{Box} "		% prettyTB t1 % " " % prettyTB t2
	MUnbox	t1 t2	-> "prim{Unbox} "	% prettyTB t1 % " " % prettyTB t2
	MTailCall  v	-> "prim{TailCall} "	% v
	MCall	v	-> "prim{Call} "	% v
	MCallApp v i	-> "prim{CallApp} "	% v % " " % i
	MApply	v	-> "prim{Apply} "	% v
	MCurry	v i	-> "prim{Curry} "	% v % " " % i
	MFun v t	-> "prim{" % v % "} "	% prettyTB t


-----------------------
-- Stmt
--
instance Pretty Stmt where
 prettyp xx
  = case xx of
	SComment s
	 -> "-- " % prettyp s

	SBind Nothing x
	 -> prettyp x


	SBind (Just v) x
	 |  length (pretty v) < 7  
	    && (not $ isXLambda x)
	    && (not $ isXLAMBDA x)
	    && (not $ isXTau x)
	 -> (padR 7 (pretty v)) 
	 	% " = " 	%> x
	 
	 | otherwise
	 -> v 	% "\n"
	  	% " =      " 	%> x  

prettyRFs (r, [])	= r % ";"
prettyRFs (r, fs)	= r %> " :- " % ", " %!% fs	% ";"

-----------------------
-- Alt
--
instance Pretty Alt where
 prettyp xx
  = case xx of
	AAlt [] x
	 -> "| otherwise \n"
	 %  "= " % x % ";"

  	AAlt (g:gs) x
	 -> "\n" %!% ("| " % g : map (\g -> ", " % g) gs)
	  % "\n"
	  % "= " % x % ";"

 
 
----------------------
-- Guard
--
instance Pretty Guard where
 prettyp xx
  = case xx of
--	GCase pat
--	 -> "|| " % pat
	
	GExp pat exp
--	 -> exp % "\n# " % pat
	 
	 -> pat	%>> " <- " % exp
	 

-----------------------
-- Pat
--
instance Pretty Pat where
 prettyp xx 
  = case xx of
  	WConst c	-> prettyp c
	

	WCon v []	-> prettyp v 
	WCon v binds
	 -> v % "\n"
	  %> ("{ " % "\n, " %!% (map prettyLVT binds))  % " }"
 
prettyLVT (label, var, t)
	= "." % label 
	% " = " % padR 5 (pretty var) 
		%> (" :: " % t)
	
----------------------
-- Label
--
instance Pretty Label where
 prettyp xx
  = case xx of
  	LIndex	i	-> prettyp i
	LVar	v	-> prettyp v


-----------------------
-- Annot
--
instance Pretty Annot where
 prettyp xx
  = case xx of
  	NString s	-> "&NString " 	 % s
	NType x		-> "&NType "  	 % x
	NTypeOp t	-> "&NTypeOp "	 % t
	NUseCount i	-> "&NUseCount " % i
	NPure		-> prettyp "&Pure"
	NBindVar v	-> "&NBindVar "	 % v
	NLevel i	-> "&NLevel "	% i
	NFreeLevel vs	-> "&NFreeLevel " % vs
	NVarSet vs	-> "@NVarSet " % Set.toList vs

-----------------------
-- Type
-- 
instance Pretty Type where
 prettyp xx
  = case xx of
	TNil 
	 -> prettyp "@TNil"
	 
	TForall v k t
	 -> let	(forallVTs, whereVTs, context, tx)	= stripSchemeT xx
	    in	"forall " % " " %!% (map fst forallVTs) % "\n.  " 
	    	% buildScheme [] whereVTs context tx

	TContext c t	-> c % " => " % t

	TWhere	t1 vts	
	 -> prettyTB t1 % "\n:- " % "\n,  " %!% [ v % " = " %> prettyTB t | (v, t) <- vts]

	TApp	t1 t2	-> prettyTB t1 % " " % prettyTB t2

	TSum	k ts	-> k % "{" % "; " %!% ts % "}"

	TMask	k t1 t2	-> t1 % " \\ " % t2

	TVar	k v	
	 -> case k of
	 	KData	-> "*" % v
		_	-> prettyp v

	-- data
	TFun x1 x2
	 -> prettyTBF x1 % " -> "  % x2

	TFunEC t1 t2 eff clo
	 -> case (eff, clo) of
	 	(TBot KEffect, 	TBot KClosure)	-> prettyTBF t1 % " -> " % t2
		(eff,   	TBot KClosure)	-> prettyTBF t1 % " -(" % eff % ")> " % t2
		(TBot KEffect,	clo)		-> prettyTBF t1 % " -(" % clo % ")> " % t2
		(eff,   	clo)		-> prettyTBF t1 % " -(" % prettyTB eff % " " % prettyTB clo % ")> " % t2

	TData v ts
	 ->       " " %!% (prettyp v : map prettyTB ts)

	-- effect
	TEffect v xs	-> " " %!% (prettyp v : map prettyTB xs)
	TBot KEffect	-> prettyp "!PURE"
	TTop KEffect	-> prettyp "!SYNC"

	-- closure
	TFree v t	-> v % " : " % t
	TTag v		-> prettyp v
	TBot KClosure	-> prettyp "$EMPTY"
	TTop KClosure	-> prettyp "$OPEN"

	-- class	
  	TClass v ts	-> v % " " % " " %!% map prettyTB ts
	
	-- kinds
	TKind    k	-> prettyp k
	
	-- wildcards
	TWild	 k	-> "(" % k % ")"
	


prettyTB t
 = case t of
	TSum{}		-> prettyp t
 	TVar{}		-> prettyp t
	TData v []	-> prettyp t
	TEffect{}	-> prettyp t
	TWild{}		-> prettyp t
	TBot{}		-> prettyp t
	TTop{}		-> prettyp t
	_		-> "(" % t % ")" 	

prettyTBF e
	| TFunEC{}	<- e
	= "(" % e % ")"
	
	| TFun{}	<- e
	= "(" % e % ")"

	| otherwise
	= prettyp e


-----------------------
-- Kind
--
instance Pretty Kind where
 prettyp xx
  = case xx of
	KNil		-> prettyp "KNil"
	KBox		-> prettyp "[]"
	KData		-> prettyp "*"
	KRegion		-> prettyp "%"
	KEffect		-> prettyp "!"
	KClosure	-> prettyp "$"
	KClass		-> prettyp "+"
	KFun k1 k2	-> k1 % " -> " % k2









