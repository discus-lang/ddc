{-# OPTIONS -fwarn-incomplete-patterns #-}

module Core.Pretty
	( pprStr )

where

import Core.Exp
import Core.Util.Bits
import Core.Util.Strip
import Type.Pretty

import qualified Shared.Var	as Var
import Shared.Var (Var)
import Shared.Error
import Shared.Pretty

import qualified Data.Set	as Set

import Util
import Util.Pretty

-----
stage	= "Core.Pretty"

-------------------------------------------------------------------------------------------------
-- Some useful options for debugging
--

-- | Fold multiple binders into a single line.
prettyFoldXLAM		= True


-------------------------------------------------------------------------------------------------
sv v		= pprStrPlain $ pv v

sb (BVar v)	= pprStrPlain $ pv v
sb (BMore v t)	= pprStrPlain $ "(" % (pprStrPlain $ pv v) % " :> " % t % ")"

-- | force display of type namespace qualifier
pv v
 = let vStrip	= v { Var.nameModule = Var.ModuleNil }
   in  case Var.nameSpace v of
 	Var.NameType	-> "*" % vStrip
	_		-> ppr vStrip

-- Top ----------------------------------------------------------------------------------------------
instance Pretty Top PMode where
 ppr xx
  = case xx of
	PNil
	 -> ppr "@PNil"

	PBind v e
	 -> v % "\n"
		% " =      " %> e  % ";\n"

	PExtern v (TBot KValue) tv
	 -> "extern " % v % ";\n"
	
	PExtern v tv to
	 -> "extern " % v % "\n"
	 %  " =      "
	 	%> (tv % "\n"
		%  ":$ " % to % ";\n")

	PExternData v k
	 -> "extern data " % v % " :: " % k % ";\n"

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

	PRegion v vts
	 -> "region " % v %> "  with {" % "; " 
	 	%!% (map (\(v, t) -> pv v % " = " % t) vts)
		% "};"

	PEffect v k
	 -> "effect " % v % " :: " % k % ";\n"

	PClass v k
	 -> "class " % v % ";\n"


	PClassDict v ts context sigs
	 -> ("class " % v <> (punc " " $ map pprPClassDict_varKind ts) <> "where\n"
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

pprPClassDict_varKind tt
 = case tt of
	TVar k v	-> parens $ v <> "::" <> k
	_		-> panic stage "pprPClassDict_varKind: no match\n"


-- CtorDef --------------------------------------------------------------------------------------------
instance Pretty CtorDef PMode where
 ppr xx
  = case xx of
  	CtorDef v fs
 	 -> v 	% "\n{\n"
	 	%> ("\n" %!% fs) % "\n}"
	 
	 
-- Exp ----------------------------------------------------------------------------------------------
instance Pretty Exp PMode where
 ppr xx
  = case xx of
	XNil	
	 -> ppr "@XNil"

	XNothing
	 -> ppr "@XNothing"

	XAnnot a e
	 -> "[" % a % ";\n " % e % "]"

	XVar v TNil
	 -> "(" % pv v % " :: _)"

	XVar v t
	 -> ifMode (elem PrettyCoreTypes)
	 	("(" % pv v % " :: " % t % ")")
		(pv v)
		
	XLAM v k e
	 | prettyFoldXLAM
	 -> let -- split off vars with simple kinds
	 	takeLAMs acc exp@(XLAM v k x)
	 	  | elem k [KRegion, KEffect, KClosure, KValue]	
		  		= takeLAMs (v : acc) x
	 
	 	takeLAMs acc exp
		 = (exp, acc)
	 
	        (xRest, vsSimple)	= takeLAMs [] xx
	    
	     in	case vsSimple of
	    	 []	-> "/\\ (" % padL 16 (sb v) % " :: " % k % ") ->\n" % e
		 _	-> "/\\  " % ", " %!% map sb (reverse vsSimple) % " ->\n" % xRest

	 | otherwise
	 -> "/\\ (" % padL 16 (sb v) % " :: " % k % ") ->\n" % e



	XLam v t x eff clo
	 -> "\\  (" % sv v % " :: " % t % ")"
		 % pEffClo % " ->\n"
		 % x
	 
	 where	pEffClo	= case (eff, clo) of 
	 			(TBot KEffect, TBot KClosure) 	-> pNil
				(TBot KEffect, _)		
				  -> "\n" % replicate 20 ' ' % " of " % prettyTB clo

				(_, TBot KClosure)		
				  -> "\n" % replicate 20 ' ' % " of " % prettyTB eff

				_ -> "\n" % replicate 20 ' ' % " of " % prettyTB eff 
				   % "\n" % replicate 20 ' ' % "    " % prettyTB clo
					 
	 
	XAPP x t
	 | spaceApp t
	 ->  x % "\n" 
	 	%> prettyTB t

	 | otherwise
	 ->  x % " " % prettyTB t

	XApp e1 e2 eff
	 -> let	pprAppLeft x 
	 	  | x =@= XVar{} || isXApp x	= ppr x
		  | otherwise			= "(" % x % ")"

		pprAppRight x
		  | x =@= XVar{} 		= " "  % x
		  | otherwise			= "\n" %> prettyExpB x

	    in	pprAppLeft e1 % pprAppRight e2

	XTau t x
	 -> "[** " % prettyTB t % " ]\n" % x

	XTet vts x
	 -> "\n" %!%
	    (map (\(v, t) -> "let " % padL 16 (sv v) % " =  " % t % " in") vts)
	    % "\n" % x	 

	XDo [s@(SBind Nothing XVar{})]
	 -> "do { " % s % "; }";
	
	XDo bs
	 -> "do {\n"
	 	%> ";\n\n" %!% bs % ";\n}"

	XMatch alts
	 -> "match {\n"
		%> ("\n\n" %!% alts)
		% "\n"
		% "}"

	XLit lit
	 -> ppr lit

	XLocal v vts x
	 -> "local " % v %> "  with {" % "; " 
	 	%!% (map (\(v, t) -> pv v % " = " % t) vts)
		% "} in\n" % x
	 

	-- prim
	XPrim m args
	 -> m % " " %> (" " %!% map prettyExpB args)

	XProject x j
	 -> x % j

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
	 -> ppr t

	XLifted vLifted vsFree
	 -> "@XLifted " % vLifted % " " % vsFree


spaceApp xx
 = case xx of
	TVar{}			-> False
	TVarMore{}		-> False
	_			-> True


prettyExpB x
 = case x of
	XVar{}		-> ppr x
	XLit{}		-> ppr x
	XAnnot{}	-> ppr x
	XType t		-> prettyTB t
	_		-> "(" % x % ")"


-- Proj --------------------------------------------------------------------------------------------
instance Pretty Proj PMode where
 ppr xx
  = case xx of
  	JField v	-> "." % v
	JFieldR v	-> "#" % v


-- Prim --------------------------------------------------------------------------------------------
instance Pretty Prim PMode where
 ppr xx 
  = case xx of
  	MSuspend v	-> "prim{Suspend} " 	% v
	MForce 		-> ppr "prim{Force}"
	MBox		-> ppr "prim{Box}"
	MUnbox		-> ppr "prim{Unbox}"
	MTailCall  	-> ppr "prim{TailCall}"	
	MCall		-> ppr "prim{Call}"
	MCallApp i	-> "prim{CallApp " % i % "}"
	MApply		-> ppr "prim{Apply} "
	MCurry	 i	-> "prim{Curry " % i % "}"
	MFun 		-> ppr "prim{Fun}"
	MOp op		-> ppr "prim{" % op % "}"


-- Op ---------------------------------------------------------------------------------------------
instance Pretty Op PMode where
 ppr xx	= ppr $ show xx


-- Stmt --------------------------------------------------------------------------------------------
instance Pretty Stmt PMode where
 ppr xx
  = case xx of
--	SComment s
--	 -> "-- " % ppr s

	SBind Nothing x
	 -> ppr x


	SBind (Just v) x
	 |  length (pprStrPlain v) < 7  
	    && (not $ isXLambda x)
	    && (not $ isXLAMBDA x)
	    && (not $ isXTau x)
	 -> (padL 7 (pprStrPlain v)) 
	 	% " = " 	%> x
	 
	 | otherwise
	 -> v 	% "\n"
	  	% " =      " 	%> x  

-- Alt --------------------------------------------------------------------------------------------
instance Pretty Alt PMode where
 ppr xx
  = case xx of
	AAlt [] x
	 -> "| otherwise \n"
	 %  "= " % x % ";"

  	AAlt (g:gs) x
	 -> "\n" %!% ("| " % g : map (\g -> ", " % g) gs)
	  % "\n"
	  % "= " % x % ";"

  
-- Guard --------------------------------------------------------------------------------------------
instance Pretty Guard PMode where
 ppr xx
  = case xx of
	GExp pat exp
	 -> pat	%> " <- " % exp
	 

-- Pat ---------------------------------------------------------------------------------------------
instance Pretty Pat PMode where
 ppr xx 
  = case xx of
	WVar v		-> pv v

  	WLit _ c	-> ppr c 

	WCon _ v []	-> pv v

	WCon _ v binds
	 -> pv v % "\n"
	  %> ("{ " % "\n, " %!% (map prettyLVT binds))  % " }"
 
prettyLVT (label, var, t)
	= "." % label 
	% " = " % pprStrPlain var
		%> (" :: " % t)
	
-- Label --------------------------------------------------------------------------------------------
instance Pretty Label PMode where
 ppr xx
  = case xx of
  	LIndex	i	-> ppr i
	LVar	v	-> ppr v


-- Annot --------------------------------------------------------------------------------------------
instance Pretty Annot PMode where
 ppr xx
  = case xx of
  	NString s	-> "&NString " 	 % s
	NType x		-> "&NType "  	 % x
	NTypeOp t	-> "&NTypeOp "	 % t
	NUseCount i	-> "&NUseCount " % i
	NPure		-> ppr "&Pure"
	NBindVar v	-> "&NBindVar "	 % v
	NLevel i	-> "&NLevel "	% i
	NFreeLevel vs	-> "&NFreeLevel " % vs
	NVarSet vs	-> "@NVarSet " % Set.toList vs


