{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Core.Pretty
	(pprStr)
where
import DDC.Main.Pretty
import DDC.Core.Exp
import DDC.Type
import DDC.Type.Data.Pretty	()
import DDC.Var
import Core.Util.Bits
import Util


-- Debugging --------------------------------------------------------------------------------------
-- | Fold multiple binders into a single line.
prettyFoldXLAM		= True


-- Utils ------------------------------------------------------------------------------------------
-- | Show a binder as a string
sb b
 = case b of
	BNil		-> "_"
	BVar v		-> pprStrPlain $ pv v
	BMore v t 	-> pprStrPlain $ parens $ (pprStrPlain $ pv v) %% ":>" %% t

-- | Show a variable as a string.
sv v	= pprStrPlain $ pv v

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
	PBind v x
	 -> v 	%! " =     " %> x 

	PExtern v (TSum k []) _
	 | k == kValue
	 -> "extern"   %% v
	
	PExtern v tv to
	 -> "extern"   %% v
	 %! " =      " %> (tv %! ":$ " % to)

	PData def
	 -> ppr def
	
	PRegion vRegion vts
	 -> pprHeadBlock ("region" %% vRegion %% "with ")
		[ pv vWit %% "=" %% t | (vWit, t) <- vts ]
	
	PEffect v k
	 -> "effect" %% v %% "::" %% k

	PClass v _
	 -> "class"  %% v

	PClassDict v vks sigs
	 -> pprHeadBlockSep 
		("class" %% v 
			 %% hsep (map pprPClassDict_varKind vks) 
			 %% "where" % nl)
		[ v' % nl %> "::" %% prettyTypeSplit sig | (v', sig) <- sigs]

	PClassInst v ts defs
	 -> pprHeadBlockSep
		("instance" %% v 
			%% hsep (map prettyTypeParens ts))
		[ v' % nl %> "= " %% x | (v', x) <- defs]


pprPClassDict_varKind (v, k)
	= parens $ v %% "::" %% k
		
	 
-- Exp ----------------------------------------------------------------------------------------------
instance Pretty Exp PMode where
 ppr xx
  = case xx of
	XNil	
	 -> ppr "@XNil"

	XVar v TNil
	 -> parens (pv v % " :: _")

	XVar v t
	 -> pprIfMode (elem PrettyCoreTypes)
	 	(parens (pv v %% "::" %% t))
		(pv v)
		
	XLAM v k e
	 | prettyFoldXLAM
	 -> let 
		-- split off vars with simple kinds
	 	takeLAMs acc (XLAM v' k' x)
	 	  | elem k' [kRegion, kEffect, kClosure, kValue]	
		  = takeLAMs (v' : acc) x
	 
	 	takeLAMs acc x
		 = (x, reverse acc)
	 
	        (xRest, vsSimple) = takeLAMs [] xx
	    
	     in	case vsSimple of
	    	 []	-> "/\\" %% parens (padL 8 (sb v) % " :: " % k)  %% "->" % nl %  e
		 _	-> "/\\" %% punc ", " (map sb vsSimple)          %% "->" % nl % xRest

	 | otherwise
	 -> "/\\" %% (parens $ padL 8 (sb v) %% "::" %% k) %% "->" % nl % e


	XLam v t x eff clo
	 -> "\\ " %% (parens $ padL 8 (sv v) %% "::" %% t)
		  %% pEffClo %% "->"
		  %! x
	 
	 where	pEffClo	
		 | eff == tPure, clo == tEmpty	
		 = blank
			
		 | eff == tPure		
		 = nl % padR 18 "of" %% prettyTypeParens clo

	 	 | clo == tEmpty		
		 = nl % padR 18 "of" %% prettyTypeParens eff

		 | otherwise
		 = nl % padR 18 "of" %% prettyTypeParens eff 
		 %! replicate 15 ' '  % "    " % prettyTypeParens clo
					 	 
	XAPP x t
	 | spaceApp t
	 ->  x % nl %> prettyTypeParens t

	 | otherwise
	 ->  x %% prettyTypeParens t

	XApp e1 e2
	 -> let	pprAppLeft x 
	 	  | x =@= XVar{} || isXApp x	= ppr x
		  | otherwise			= parens x

		pprAppRight x
		  | x =@= XVar{} 		= " " % x
		  | otherwise			= nl  %> prettyExpB x

	    in	pprAppLeft e1 % pprAppRight e2

	XTau t x
	 -> "[**" %% prettyTypeParens t %% "]" %! x

	XDo bs
	 -> pprHeadBlock    "do " bs

	XMatch alts
	 -> pprHeadBlockSep "match " alts
	
	XLit lit
	 -> ppr lit

	XLocal v vts x
	 -> "local" 	%% v 	%> " with"
	 			%% braces (punc "; "  [pv v' % " = " % t | (v', t) <- vts])
				%% "in"
			%! x
	 
	XPrim m args
	 -> m %> (hsep $ map prettyExpB args)

	XPrimType t
	 -> prettyTypeParens t


spaceApp xx
 = case xx of
	TVar{}			-> False
	_			-> True


prettyExpB x
 = case x of
	XVar{}		-> ppr x
	XLit{}		-> ppr x
	XPrimType t	-> prettyTypeParens t
	_		-> parens x


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
	PrimCallSuperApply i	-> ppr "prim{SuperApply " % i % "}"
	PrimCallApply		-> ppr "prim{Apply} "
	PrimCallCurry i		-> ppr "prim{Curry " % i % "}"
	

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
	 | isXLambda x
	 , isXLAMBDA x
	 -> sv v %! " =     " %> x 

	 | length (pprStrPlain v) < 12
	 , not $ isXLambda x
	 , not $ isXLAMBDA x
	 , not $ isXTau x
	 -> padL 12 (sv v) %% "=" %> x
	 
	 | otherwise
	 -> sv v %> "=" %% x  

-- Alt --------------------------------------------------------------------------------------------
instance Pretty Alt PMode where
 ppr xx
  = case xx of
	AAlt [] x
	 -> "| DEFAULT"
	 %! "=" %% x

  	AAlt gs x
	 -> "|"  %% punc (nl % ", ") gs
	 %! "="  %% x


-- Guard --------------------------------------------------------------------------------------------
instance Pretty Guard PMode where
 ppr xx
  = case xx of
	GExp pat x@XVar{}	-> pat %% "<-" %% x
	GExp pat x		-> pat %! indent ("<-" %% x)


-- Pat ---------------------------------------------------------------------------------------------
instance Pretty Pat PMode where
 ppr xx 
  = case xx of
	WVar v		-> pv  v
  	WLit _ c	-> ppr c
	WCon _ v []	-> pv  v

	WCon _ v binds
	 -> pv v  %% "{" 
		  %! indent (punc (semi % nl) (map prettyLVT binds) % semi) %% "}"


prettyLVT (label, var, t)
	=  "." %  label 
	%% "=" %% padL 8 (sv var) %> " ::" %% t


-- Label --------------------------------------------------------------------------------------------
instance Pretty Label PMode where
 ppr xx
  = case xx of
  	LIndex	i	-> ppr i
	LVar	v	-> ppr v

