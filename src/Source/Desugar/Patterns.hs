
module Source.Desugar.Patterns
(
	matchPats,
	rewritePatVar,
	sprinkleAtsG,
	liftAtsG,
	rewritePatTreeM
)

where

import Util

import qualified Shared.Var	as Var
import qualified Shared.VarUtil	as Var

import Shared.Var	(Var, NameSpace(..))
import Shared.VarPrim
import Shared.Base

import qualified Source.Exp		as S
import qualified Source.Util		as S

import qualified Desugar.Exp		as D
import qualified Desugar.Plate.Trans	as D

import Source.Desugar.Base

-----
matchPats 
	:: SourcePos
	-> [S.Exp]
	-> (D.Exp Annot)
	-> RewriteM (D.Exp Annot)
	
matchPats sp pp x
 = do	pp'		<- mapM (sprinkleAts sp) pp
 	(vs, crush)	<- crushPats  sp pp' x
	return		$ lambdaPats sp vs crush

lambdaPats sp [] x	= x
lambdaPats sp (v:vs) x	= D.XLambda sp v (lambdaPats sp vs x)
 
crushPats sp []	    x
 = 	return ([], x)
 
crushPats sp (p:ps) x
 = do 	(vs, x2)	<- crushPats sp ps x
	(x3, v)		<- crushPat  sp x2 p
	return	(v : vs, x3)
	

crushPat 
	:: SourcePos 
	-> D.Exp Annot
	-> S.Exp
	-> RewriteM (D.Exp Annot, Var)

crushPat sp x p
 = case p of
 	S.XAt sp1 v (S.XTuple sp ps)
	 -> do	(x', vs)	<- mapAccumLM (crushPat sp) x (reverse ps)

		let lvs		= [ (D.LIndex sp i, v)	
					| i	<- [0..]
					| v	<- reverse vs]

		return	( D.XMatch sp 
				(Just (D.XVar sp v))
				[D.AAlt sp 	[D.GCase sp (D.WConLabel sp (primTuple  $ length ps)	lvs)]
						x']
			, v )

	S.XAt sp1 v (S.XCons sp p1 p2)
	 -> do	(x2, v2)	<- crushPat sp x  p2
	 	(x1, v1)	<- crushPat sp x2 p1

		let lvs		= [ (D.LIndex sp i, v)	
					| i	<- [0..]
					| v	<- [v1, v2]]

		return	( D.XMatch sp 
				(Just (D.XVar sp v))
				[D.AAlt sp 	[D.GCase sp (D.WConLabel sp primCons lvs)]
						x1] 
			, v)
		
	S.XAt sp1 v (S.XCon sp var ps)
	 -> do	(x', vs)	<- mapAccumLM (crushPat sp) x (reverse ps)

		let lvs		= [ (D.LIndex sp i, v)	
					| i	<- [0..]
					| v	<- reverse vs]

	 	return	( D.XMatch sp 
				(Just (D.XVar sp v))
				[D.AAlt sp 	[D.GCase sp (D.WConLabel sp var lvs)]
						x']
			, v)
			
	S.XVar sp v
	 -> 	return	( x, v )






sprinkleAts :: SourcePos -> S.Exp -> RewriteM S.Exp
sprinkleAts sp p
 = case p of
 	S.XVar sp var
	 | Var.isCtorName var
	 -> do	v	<- newVarN NameValue
	 	return	$ S.XAt sp v (S.XCon sp var [])
		
	 | otherwise
	 ->	return	$ S.XVar sp var
 
 	S.XCons sp x1 x2
	 -> do	v	<- newVarN NameValue
	 	x1'	<- sprinkleAts sp x1
		x2'	<- sprinkleAts sp x2
		return	$ S.XAt sp v (S.XCons sp x1' x2')
 
 	S.XTuple sp xx
	 -> do	v	<- newVarN NameValue
	 	xx'	<- mapM (sprinkleAts sp) xx
		return	$ S.XAt sp v (S.XTuple sp xx')
		
	-- convert from defixed pattern into XCon form.
	S.XApp{} 
	 -> do	v	<- newVarN NameValue
	 	let (S.XVar sp var : xs)	= S.flattenApps p
	 	xs'		<- mapM (sprinkleAts sp) xs
		let var'	=  rewritePatVar var
		return	$ S.XAt sp v (S.XCon sp var' xs')
			


rewritePatVar v
 = case Var.name v of 
 	":"	-> primCons 	{ Var.info = Var.info v }
	"++"	-> primAppend 	{ Var.info = Var.info v }
	_	-> v



-----------------------------------------------------------------
-- sprinkleAts
--	Add new @ vars to give names to internal patterns.
--

sprinkleAtsG
	:: SourcePos
	-> D.Guard Annot -> RewriteM (D.Guard Annot)
	
sprinkleAtsG sp gg
 = case gg of
 	D.GCase nn p
	 -> do	p'	<- sprinkleAtsW_down sp p
	 	return	$ D.GCase nn p'
		
	D.GExp nn p x
	 -> do	p'	<- sprinkleAtsW_down sp p
	 	return	$ D.GExp nn p' x


sprinkleAtsW 
	:: SourcePos 
	-> D.Pat Annot -> RewriteM (D.Pat Annot)

sprinkleAtsW sp ww
 = case ww of
	D.WConLabel nn var lvs
	 -> do	v	<- newVarN NameValue
	 	return	$ D.WAt nn v (D.WConLabel nn var lvs)
		
	D.WConst nn c
	 ->	return	ww

	D.WVar nn var	
	 -> 	return $ D.WVar nn var
	
 	D.WConLabelP nn var lws
	 -> do	v	<- newVarN NameValue
	 	lws'	<- mapZippedM 
	 			return
				(sprinkleAtsW sp)
				lws
				
		return	$ D.WAt nn v (D.WConLabelP nn var lws')
		
	D.WWildcard nn 
	 ->	return	$ D.WWildcard nn
	 
	D.WAt nn v w	
	 -> do	w'	<- sprinkleAtsW_down sp w
	 	return	$ D.WAt nn v w'

sprinkleAtsW_down sp ww
 = case ww of
 	D.WConLabel nn var lvs
	 -> do	return	$ D.WConLabel nn var lvs
	
	D.WConLabelP nn v lws
	 -> do	lws'	<- mapZippedM
	 			return
				(sprinkleAtsW sp)
				lws
		return	$ D.WConLabelP nn v lws'
		
	_ 	-> return ww
	

-----
type CollectAtS 	= [(Var, D.Pat Annot)]
type CollectAtM		= State CollectAtS

liftAtsG :: D.Guard Annot -> [D.Guard Annot]
liftAtsG xx	
 = let	(g, vws)= runState 
 			(D.transZM ((D.transTableId return) { D.transW = collectAtNodesW }) xx)
			[]
			
	gs	= g : [ D.GExp none p (D.XVar none v) | (v, p) <- vws] 
	
	gs'	= map (D.transformW 
			(\w -> case w of
				D.WConLabelP nn v lws 
				 -> let	lvs	= mapZipped id (\(D.WVar nn v) -> v) lws
				    in	D.WConLabel nn v lvs
				    
				_ -> w))
			gs
   in	gs'
	

collectAtNodesW :: D.Pat Annot -> CollectAtM (D.Pat Annot)
collectAtNodesW ww
 = case ww of
 	D.WAt nn v w
	 -> do	s	<- get
	 	put	((v, w) : s)
		return	$ D.WVar nn v

	D.WConLabelP nn v lws
	 -> do	let lvs	= mapZipped id (\(D.WVar _ v) -> v) lws
	 	return	$ D.WConLabel nn v lvs
		
	_ -> return ww
	

rewritePatTreeM
	:: D.Tree Annot -> RewriteM (D.Tree Annot)

rewritePatTreeM tree
	= mapM (D.transformXM rewritePatX) tree

rewritePatX 
	:: D.Exp Annot -> RewriteM (D.Exp Annot)

rewritePatX xx
 = case xx of
 	D.XMatch nn co aa
	 -> do	aa'	<- mapM (rewritePatA co) aa
	 	return	$ D.XMatch nn co aa'

	_ -> return xx
		
rewritePatA co aa
 = case aa of
	D.AAlt nn gs x
	 -> do	gsAts		<- mapM (sprinkleAtsG none) gs
	  	let gsLift	= catMap liftAtsG gsAts
		return	$ D.AAlt none gsLift x



