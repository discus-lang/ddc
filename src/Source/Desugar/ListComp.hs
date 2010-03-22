
module Source.Desugar.ListComp
	(rewriteListComp)
where
import Util
import Shared.VarPrim
import Shared.Error
import Shared.Pretty
import Source.Desugar
import Source.Desugar.Base
import Source.Desugar.Patterns
import DDC.Base.SourcePos
import qualified DDC.Var.PrimId		as Var
import qualified Shared.Var 		as Var
import qualified Source.Exp		as S
import qualified Desugar.Exp		as D

stage = "Source.Desugar.ListComp"

-----
-- rewriteListComp
--	Expand out a list comprehension.
--	ala Section 3.11 of the Haskell 98 report.
--
--	BUGS: let patterns not implemented yet.
--
rewriteListComp 
	:: S.Exp SourcePos -> RewriteM (D.Exp Annot)
	
rewriteListComp x 
 = case x of

	-- [ e | True ] 		=> [e]
 	S.XListComp sp exp [S.LCExp (S.XVar _ v)]
	 |  Var.varId v == Var.VarIdPrim Var.VTrue 	
	 -> do	exp'	<- rewrite exp
	 	return 	$ D.XApp sp (D.XApp sp (D.XVar sp primCons) exp') (D.XVar sp primNil)
	 
	-- [ e | q ]			=> [e | q, True]
	S.XListComp sp exp [q]		
	 -> rewriteListComp 
	 		$ S.XListComp sp exp [q, S.LCExp (S.XVar sp primTrue)]
	
	-- [ e | b, Q ]			=> if b then [e | Q] else []
	S.XListComp sp exp (S.LCExp b : qs)
	 -> do	lc'	<- rewriteListComp $ S.XListComp sp exp qs
		b'	<- rewrite b
	 	return 	$ D.XIfThenElse sp b' lc' (D.XVar sp primNil)
	
	-- [ e | p <- l, Q]		=> let ok p = [e | Q] in concatMap ok l
	S.XListComp sp exp (S.LCGen lazy (S.WVar _ p) l : qs)
	 -> do
		let catMapVar	= if lazy then primConcatMapL else primConcatMap;

		lc'	<- rewriteListComp $ S.XListComp sp exp qs
		l'	<- rewrite l
		
	 	return	$ D.XDo sp
				[ D.SBind sp Nothing  (D.XApp sp (D.XApp sp (D.XVar sp catMapVar) (D.XLambda sp p lc') ) l') ]

	-- [e | pattern <- l, Q]		=> 
	S.XListComp sp exp (S.LCGen lazy pat l : qs)
	 -> do
		let catMapVar	= if lazy then primConcatMapL else primConcatMap

		lc'	<- rewriteListComp $ S.XListComp sp exp qs
		l'	<- rewrite l
		pat'	<- rewrite [pat]
		patFunc	<- makeMatchFunction sp pat' lc'

	 	return	$ D.XDo sp
				[ D.SBind sp Nothing  (D.XApp sp (D.XApp sp (D.XVar sp catMapVar) patFunc) l') ]


	_ -> panic stage
		$ pprStrPlain $ "rewriteListComp failed for\n    " % x % "\n"

