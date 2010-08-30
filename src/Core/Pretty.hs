{-# OPTIONS -fwarn-incomplete-patterns #-}

module Core.Pretty
	(pprStr)
where
import DDC.Main.Pretty
import DDC.Core.Exp
import DDC.Type
import DDC.Type.Data.Base
import DDC.Type.Data.Pretty	()
import DDC.Var
import qualified Data.Map	as Map
import Data.Function
import Core.Util.Bits
import Util


-- Debugging --------------------------
-- | Fold multiple binders into a single line.
prettyFoldXLAM		= True


-------------------------------------------------------------------------------------------------
sv v		= pprStrPlain $ pv v

sb (BNil)	= "_"
sb (BVar v)	= pprStrPlain $ pv v
sb (BMore v t)	= pprStrPlain $ "(" % (pprStrPlain $ pv v) % " :> " % t % ")"

-- | force display of type namespace qualifier
pv v
 = let vStrip	= v { varModuleId = ModuleIdNil }
   in  case varNameSpace v of
 	NameType	-> "*" % vStrip
	_		-> ppr vStrip


-- Top ----------------------------------------------------------------------------------------------
instance Pretty Top PMode where
 ppr xx
  = case xx of
	PBind v e
	 -> v % "\n"
		% " =      " %> e  % ";\n"

	PExtern v (TSum k []) tv
	 | k == kValue
	 -> "extern " % v % ";\n"
	
	PExtern v tv to
	 -> "extern " % v % "\n"
	 %  " =      "
	 	%> (tv % "\n"
		%  ":$ " % to % ";\n")

	PExternData v k
	 -> "extern data " % v % " :: " % k % ";\n"

	PData (DataDef 
		{ dataDefName	= v
		, dataDefCtors	= ctors })
	 | Map.null ctors
	 -> "data " % " " % ppr v % ";\n" 

	 | otherwise
	 -> let ctorsList = sortBy (compare `on` ctorDefTag) $ Map.elems ctors
	    in  "data" <> v <> "where\n"
	 	% "{\n" 
	 	%> (vvcat ctorsList % "\n")
		% "}\n"

	PRegion v vts
	 -> "region " % v %> "  with {" 
		% punc "; " (map (\(v, t) -> pv v % " = " % t) vts)
		% "};"

	PEffect v k
	 -> "effect " % v % " :: " % k % ";\n"

	PClass v k
	 -> "class " % v % ";\n"


	PClassDict v vks sigs
	 -> ("class " % v <> (punc " " $ map pprPClassDict_varKind vks) <> "where\n"
	 	% "{\n"
		%> (";\n\n" %!% map (\(v, sig) -> v % "\n ::     " %> sig) sigs)
		% "\n}\n")

	PClassInst v ts defs
	 -> ("instance " % v % " " % " " %!% map prettyTypeParens ts % "\n"
			% "{\n"
			%> ("\n\n" %!% (map (\(v, exp) 
						-> v % "\n" 
						% " =      " %> exp % ";") defs))
			% "\n"
			% "}\n\n")

pprPClassDict_varKind (v, k)
	= parens $ v <> "::" <> k
		
	 
-- Exp ----------------------------------------------------------------------------------------------
instance Pretty Exp PMode where
 ppr xx
  = case xx of
	XNil	
	 -> ppr "@XNil"

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
	 	  | elem k [kRegion, kEffect, kClosure, kValue]	
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
	 
	 where	pEffClo	
		 = case (eff, clo) of 

	 		(eff, clo)
			 | eff == tPure, clo == tEmpty	
			 -> blank
			
			(eff, _)
			 | eff == tPure		
			 -> "\n" % replicate 20 ' ' % " of " % prettyTypeParens clo

			(_, clo)
			 | clo == tEmpty		
			  -> "\n" % replicate 20 ' ' % " of " % prettyTypeParens eff

			_ -> "\n" % replicate 20 ' ' % " of " % prettyTypeParens eff 
			   % "\n" % replicate 20 ' ' % "    " % prettyTypeParens clo
					 	 
	XAPP x t
	 | spaceApp t
	 ->  x % "\n" 
	 	%> prettyTypeParens t

	 | otherwise
	 ->  x % " " % prettyTypeParens t

	XApp e1 e2
	 -> let	pprAppLeft x 
	 	  | x =@= XVar{} || isXApp x	= ppr x
		  | otherwise			= "(" % x % ")"

		pprAppRight x
		  | x =@= XVar{} 		= " "  % x
		  | otherwise			= "\n" %> prettyExpB x

	    in	pprAppLeft e1 % pprAppRight e2

	XTau t x
	 -> "[** " % prettyTypeParens t % " ]\n" % x

	XDo [s@(SBind Nothing XVar{})]
	 -> "do { " % s % "; }";
	
	XDo bs
	 -> "do {\n"
	 	%> ";\n\n" %!% bs % ";\n}"

	XMatch alts
	 -> "match {\n"
		%> vvcat alts
		% "\n"
		% "}"

	XLit lit
	 -> ppr lit

	XLocal v vts x
	 -> "local " % v %> "  with {" % "; " 
	 	%!% (map (\(v, t) -> pv v % " = " % t) vts)
		% "} in\n" % x
	 
	XPrim m args
	 -> m % " " %> (" " %!% map prettyExpB args)

	XPrimType t
	 -> parens $ ppr t


spaceApp xx
 = case xx of
	TVar{}			-> False
	_			-> True


prettyExpB x
 = case x of
	XVar{}		-> ppr x
	XLit{}		-> ppr x
	_		-> "(" % x % ")"




-- Prim --------------------------------------------------------------------------------------------
instance Pretty Prim PMode where
 ppr xx 
  = case xx of
	MForce 		-> ppr "prim{Force}"
	MBox		-> ppr "prim{Box}"
	MUnbox		-> ppr "prim{Unbox}"
	MOp op		-> ppr "prim{" % op % "}"
	MCall call	-> ppr call


-- PrimCall ----------------------------------------------------------------------------------------
instance Pretty PrimCall PMode where
 ppr xx
  = case xx of
	PrimCallTail  		-> ppr "prim{TailCall}"	
	PrimCallSuper		-> ppr "prim{SuperCall}"
	PrimCallSuperApply i	-> "prim{SuperApply " % i % "}"
	PrimCallApply		-> ppr "prim{Apply} "
	PrimCallCurry i		-> "prim{Curry " % i % "}"
	

-- PrimOp ------------------------------------------------------------------------------------------
instance Pretty PrimOp PMode where
 ppr xx	= ppr $ show xx


-- Stmt --------------------------------------------------------------------------------------------
instance Pretty Stmt PMode where
 ppr xx
  = case xx of
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
	 -> "| " % g % (punc (ppr ",  ") $ map ppr gs)
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

	WCon _ v []	-> pv v % "{}"

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

