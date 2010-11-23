{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- | Pretty printing for desugared source.
module DDC.Desugar.Pretty
	(stripAnnot)
where
import DDC.Desugar.Transform
import DDC.Type.Data.Pretty	()
import DDC.Desugar.Exp
import DDC.Main.Error
import DDC.Main.Pretty		
import DDC.Type
import DDC.Var

stage = "Desugar.Pretty"

stripAnnot xx	
 = transformN (\_ -> Nothing :: Maybe ()) xx

annot nn x
 = case nn of
 	Nothing	-> x
	Just n	-> "[" % n % ": " % x % "]"

pprVar_unqual var
 = ppr $ var { varModuleId = ModuleIdNil }


-- Top -------------------------------------------------------------------------
instance Pretty a PMode => Pretty (Top (Maybe a)) PMode where
 ppr xx
  = case xx of
	PImport nn ms
	 -> annot nn $ pprHeadBlock "import" ms

	PExtern nn v tv (Just to)
	 -> let mSeaName	= takeSeaNameOfVar v
		pName		= case mSeaName of 
					Nothing -> ppr " "
					Just s  -> ppr $ show s
		
	    in annot nn
	 	(ppr"foreign import" %% pName %% v % nl
		%>  vcat [ "::" %% prettyTypeSplit tv
			 , ":$" %% to % semi])

	PExtern nn v tv Nothing
	 -> annot nn $ "extern " % v  %> (nl % "::" %% prettyTypeSplit tv % ";")
		
	-- super sigs
	PSuperSig nn v k
	 -> annot nn $ "class"	%% v %% "::" % k % ";"
 
	-- kind sigs
	PKindSig nn v k
	 | resultKind k == kValue	
	 -> annot nn $ "data"	%% v %% "::" %% k % ";"

 	 | resultKind k == kEffect
  	 -> annot nn $ "effect"	%% v %% "::" %% k % ";"

	 | otherwise	
	 -> annot nn $ "type"	%% v %% "::" %% k % ";"

	-- types
	PTypeSynonym nn v t
	 -> annot nn $ "type"	%% v %% "=" %% t % ";"

	PRegion nn v
	 -> annot nn $ "region" %% v % ";"

	PData nn ddef
	 -> annot nn $ ppr ddef

	-- data classes
	PClassDecl nn v ts sigs
	 -> annot nn $ pprHeadBlockSep 
		("class" %% pprVar_unqual v 
			 %% hsep (map pprPClassDict_varKind ts)
		 	 %% "where" % nl)
	  $ [ pprVar_unqual v' % nl % "        :: " %> prettyTypeSplit sig
			| (v', sig)	<- sigs]
	

	PClassInst nn v ts ss
	 -> annot nn $ pprHeadBlock
		("instance " %% v 
			%% hsep (map prettyTypeParens ts) 
			%% "where" % nl)
		ss

	-- projections
	PProjDict nn t ss
	 -> annot nn $ pprHeadBlock
	 	("project" %% t %% "where" % nl)
		ss

	PTypeSig nn sigMode vs t
	 -> annot nn $ punc ", " vs % nl %> (sigMode %% prettyTypeSplit t) % ";"

	PBind nn v x
	 -> annot nn $ v % nl %% "=" %> x % ";"


pprPClassDict_varKind tt
 = case tt of
	TVar k (UVar v)	-> parens $ v %% "::" %% k
	_		-> panic stage "pprPClassDict_varKind: no match"


-- Exp -------------------------------------------------------------------------
instance Pretty a PMode => Pretty (Exp (Maybe a)) PMode where
 ppr xx	
  = case xx of
  	XNil		-> ppr "@XNil"
	XVoid	nn	-> annot nn $ ppr "@XVoid"
	XLit	nn l	-> annot nn $ ppr l
	XVar	nn v	-> annot nn $ ppr v
	XVarInst nn v	-> annot nn $ "@XVarInst" %% v
	XProj 	nn x j	-> annot nn $ "@XProj"    %% prettyExpParens x  %% j
	XProjT	nn t j	-> annot nn $ "@XProjT"   %% prettyTypeParens t %% j

	XLambda    	nn v x 		
	 -> annot nn $ "\\" % v  %% "->" % nl %  x

	XLambdaTEC 	nn v x t eff clo 
	 -> annot nn $ "\\" % v  %% parens (eff %% clo) %% "::" %% t %% "->" % nl % x

	XApp	Nothing x1 x2	
	 -> x1 %% prettyExpParens x2

	XApp	nn x1 x2	
	 -> annot nn $ x1 %> (nl % prettyExpParens x2)

	XMatch  nn Nothing aa	
	 -> annot nn $ pprHeadBlockSep "match " aa
	
	XMatch  nn (Just x1) aa	
	 -> annot nn $ pprHeadBlockSep ("match" %% x1 %% "with ") aa
	 
	XDo	nn ss
	 -> annot nn $ pprHeadBlock "do " ss
	 
	XIfThenElse nn x1 x2 x3
	 -> annot nn $ "if" %% x1 % nl 
				%> (" then" %% x2) % nl
				%> (" else" %% x3)

	XProjTagged 	nn vI tC x j	
	 -> annot nn $ "@XProjTagged"  %% vI %% tC %% prettyExpParens x %% j

	XProjTaggedT 	nn vI tC j
	 -> annot nn $ "@XProjTaggedT" %% vI %% tC % j


-- | Pretty print an expression, wrapping it parens if it's not atomic.
prettyExpParens :: Pretty (Exp a) PMode => Exp a -> Str 
prettyExpParens xx
 = case xx of
	XLambda{}	-> nl %> parens xx
	XVar{}		-> ppr xx
	_		-> parens xx


-- Proj ------------------------------------------------------------------------
instance Pretty a PMode => Pretty (Proj (Maybe a)) PMode where
 ppr xx
  = case xx of
  	JField  nn v	-> annot nn ("." % v)
	JFieldR nn v	-> annot nn ("#" % v)
	

-- Stmt ------------------------------------------------------------------------
instance Pretty a PMode => Pretty (Stmt (Maybe a)) PMode where
 ppr xx
  = case xx of
	SBind nn Nothing x
	 -> annot nn $ ppr x

	SBind nn (Just v) x@XLambda{}
	 -> let v'	= v { varModuleId = ModuleIdNil}
	    in  annot nn $ v' % nl %% " =" %> x

	SBind nn (Just v) x
	 -> let v'	= v { varModuleId = ModuleIdNil}
	    in  annot nn $ v' 	%> (" ="  %% x)

	SBindMonadic nn pat x
	 -> annot nn $ pat 	%> (" <- " % x)
	
	SBindPat nn pat x
	 -> annot nn $ pat 	%> (" = "  % x)
	
	SSig  nn sigMode vs  t	
	 -> let vs'	= map (\v -> v { varModuleId = ModuleIdNil}) vs
	    in	annot nn (vs' %!% ", " %>> (ppr sigMode %% t))


-- Alt -------------------------------------------------------------------------
instance Pretty a PMode => Pretty (Alt (Maybe a)) PMode where
 ppr xx
  = case xx of
	AAlt	 nn [] x	
	 -> annot nn $ "\\= " % x

	AAlt	 nn gs x	
	 -> annot nn $ "|" %% punc (nl % ", ") gs % nl % "=" %% x


-- Guard -----------------------------------------------------------------------
instance Pretty a PMode => Pretty (Guard (Maybe a)) PMode where
 ppr gg
  = case gg of
  	GCase nn pat	-> annot nn (pat %% "<-")
	GExp  nn pat x	-> annot nn (pat %> "<-" %% x)
	

-- Pat ------------------------------------------------------------------------
instance Pretty a PMode => Pretty (Pat (Maybe a)) PMode where
 ppr ww
  = case ww of
	WConLabel nn v lvs	
	 -> annot nn $ v %% braces (punc ", " [l % " = " % v' | (l, v') <- lvs])

	WLit nn l 	-> annot nn (ppr l)
	WVar nn v 	-> annot nn (ppr v)
	WAt nn v w	-> annot nn (v % "@" % parens w)

	WConLabelP nn v lvs	
	 -> annot nn $ v %% " (--pat--)"
			% braces (punc ", " [l % " = " % v' | (l, v') <- lvs])


-- Label -----------------------------------------------------------------------
instance Pretty a PMode => Pretty (Label (Maybe a)) PMode where
 ppr ll
  = case ll of
  	LIndex	nn i		-> annot nn ("." % i)
	LVar	nn v		-> annot nn ("." % v)
	
