
module Source.Desugar.ListComp
(
	rewriteListComp
)

where

import qualified Shared.Var as Var
import Shared.Var	(NameSpace(..))
import Shared.VarPrim

import qualified Source.Exp		as S
import qualified Desugar.Exp		as D

import Source.Desugar
import Source.Desugar.Base

-----
-- rewriteListComp
--	Expand out a list comprehension.
--	ala Section 3.11 of the Haskell 98 report.
--
--	BUGS: let patterns not implemented yet.
--
rewriteListComp 
	:: S.Exp -> RewriteM (D.Exp Annot)
	
rewriteListComp x 
 = case x of

	-- [ e | True ] 		=> [e]
 	S.XListComp sp exp [S.LCExp (S.XVar _ v)]
	 |  Var.bind v == Var.VTrue 	
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
	S.XListComp sp exp (S.LCGen lazy (S.XVar _ p) l : qs)
	 -> do
		let catMapVar	= if lazy then primConcatMapL else primConcatMap;

		v	<- newVarNI NameValue [Var.ISourcePos sp]
		lc'	<- rewriteListComp $ S.XListComp sp exp qs
		l'	<- rewrite l
		
	 	return	$ D.XDo sp
				[ D.SBind sp (Just v) (D.XLambda sp p lc')
				, D.SBind sp Nothing  (D.XApp sp (D.XApp sp (D.XVar sp catMapVar) (D.XVar sp v) ) l') ]
