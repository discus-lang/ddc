
module Source.Desugar.Patterns
	( rewritePatVar
	, rewritePatternsTreeM
	, makeMatchFunction
	, mergeBindings)

where

import Util

import qualified Shared.Var	as Var
import qualified Shared.VarUtil	as Var

import Shared.Var	(Var, NameSpace(..))
import Shared.VarPrim
import Shared.Base
import Shared.Error
import Shared.Pretty

import qualified Source.Exp		as S
import qualified Source.Util		as S
import qualified Source.Error		as S

import Desugar.Util			as D
import Desugar.Exp			as D
import Desugar.Bits			as D
import Desugar.Plate.Trans		as D
import Desugar.Pretty			as D


import Source.Desugar.Base
import Source.Pretty


import qualified Data.Map		as Map
import Data.Map				(Map)

import Debug.Trace		

-- stage	= "Source.Desugar.Patterns"

-- rewritePatTree ----------------------------------------------------------------------------------

rewritePatternsTreeM
	:: D.Tree Annot -> RewriteM (D.Tree Annot)

rewritePatternsTreeM tree
 = do	
 	-- expand out patterns in match expressions
	tree'	<- mapM (D.transformXM rewritePatX) tree

	-- merge bindings at top level
	let (psBind, psRest)	= partition (=@= PBind{}) tree'
	let (ssMerged, errs)	= mergeBindings $ map topBindToStmt psBind
	let psMerged		= map stmtToTopBind ssMerged
		
	when (not $ isNil errs)
	 $ dieWithUserError errs
	 
	return	$ psRest ++ psMerged

topBindToStmt (PBind n mV x)	= SBind n mV x
stmtToTopBind (SBind n mV x)	= PBind n mV x

rewritePatX 
	:: D.Exp Annot -> RewriteM (D.Exp Annot)

rewritePatX xx
 = case xx of
 	D.XMatch nn co aa
	 -> do	aa'	<- mapM (rewritePatA co) aa
	 	return	$ D.XMatch nn co aa'

	D.XDo nn ss
	 -> do	let (ss', errs)	= mergeBindings ss
		
		when (not $ null errs)
		 $ dieWithUserError errs
		
	   	return	$ D.XDo nn ss'

	_ -> return xx
		
rewritePatA co aa
 = case aa of
	D.AAlt sp gs x
	 -> do	gsAts		<- mapM (sprinkleAtsG sp) gs
	  	let gsLift	= catMap simplifyGuard gsAts
		return	$ D.AAlt sp gsLift x


-- | These variables are treated as special aliases for list functions.
-- 	TODO: 	It would be better to define these in the source program, or allow
--		symbolic constructor names like in Haskell.
--
rewritePatVar :: Var -> Var
rewritePatVar v
 = case Var.name v of 
 	":"	-> primCons 	{ Var.info = Var.info v }
	"++"	-> primAppend 	{ Var.info = Var.info v }
	_	-> v

-- Name intermediate pattern nodes in a guard
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

-- Name internal nodes in this pattern
sprinkleAtsW 
	:: SourcePos 
	-> D.Pat Annot -> RewriteM (D.Pat Annot)

sprinkleAtsW sp ww
 = case ww of
	D.WConLabel nn var lvs
	 -> do	v	<- newVarN NameValue
	 	return	$ D.WAt nn v ww
		
	D.WConst nn c
	 -> do	v	<- newVarN NameValue
	 	return	$ D.WAt nn v ww

	D.WVar nn var	
	 -> 	return $ D.WVar nn var
	
 	D.WConLabelP nn var lws
	 -> do	v	<- newVarN NameValue
	 	lws'	<- mapZippedM 
	 			return
				(sprinkleAtsW sp)
				lws
				
		return	$ D.WAt nn v (D.WConLabelP nn var lws')
		
	D.WAt nn v w	
	 -> do	w'	<- sprinkleAtsW_down sp w
	 	return	$ D.WAt nn v w'


-- name internal nodes in thie pattern, but not the outer one.
sprinkleAtsW_down sp ww
 = case ww of
 	D.WConLabel nn var lvs
	 -> do	return	$ D.WConLabel nn var lvs
	
	D.WConst nn l
	 ->	return ww
	
	D.WVar nn v
	 ->	return ww
	
	D.WAt nn v w
	 -> do	w'	<- sprinkleAtsW_down nn w
	 	return	$ D.WAt nn v w'

	D.WConLabelP nn v lws
	 -> do	lws'	<- mapZippedM
	 			return
				(sprinkleAtsW nn)
				lws
		return	$ D.WConLabelP nn v lws'


-- simplifyGuard -----------------------------------------------------------------------------------

-- A list of vars and what pattern to match them against
type CollectAtS 	= [(Var, D.Pat Annot, SourcePos)]
type CollectAtM		= State CollectAtS


-- Simplify this guard so that all matches are against simple patterns
--	
-- eg	simplifyGuard (Cons v2@(Tuple2 v3 v4) v5@(Maybe v6@Tuple2 v7 v8)) <- v0)
--
-- => 		Cons   v2 v5	<- v0
--		Tuple2 v3 v4	<- v2
--		Maybe  v6	<- v5
--		Tuple2 v7 v8	<- v6 
--		
simplifyGuard :: D.Guard Annot -> [D.Guard Annot]
simplifyGuard guard
 = let	
 	-- decend into the guard, collecting up the simple patterns
 	(g, vws)= runState 
 			(D.transZM ((D.transTableId return) 
					{ D.transW = collectAtNodesW }) guard)
			[]

	gs	= g 					-- the outer pattern is returned
		: [ D.GExp sp p (D.XVar sp v) 		-- build a match for each internal node
				| (v, p, sp) <- vws]
  in	gs

collectAtNodesW :: D.Pat Annot -> CollectAtM (D.Pat Annot)
collectAtNodesW ww
 = case ww of
 	D.WAt sp v w
	 -> do	s	<- get
	 	put	((v, w, sp) : s)
		return	$ D.WVar sp v

	D.WConLabelP nn v lws
	 ->  do	let lvs	= mapZipped 
	 			id 
				(\w -> case w of
	 				D.WVar _ v 	-> v)
				lws

	 	return	$ D.WConLabel nn v lvs
		
	_ -> return ww
	


-- makeMatchFunction -------------------------------------------------------------------------------

-- | Build a function that matches on the given pattern expressions then returns the result
--
-- eg 	makeMatchFunction  [(x, y) : xs, Just 3]
--
-- => 	\v1 v2  
--	  -> match { 
--		| (v1, y) : xs 	<- v1 
--		| Just 3	<- v2
--	  }
--
--	where v1 and v2 are fresh variables
--		

makeMatchFunction
	:: SourcePos			-- ^ source position to use on new expression nodes
	-> [D.Pat Annot]		-- ^ patterns to match against
	-> D.Exp Annot			-- ^ result expression
	-> RewriteM (D.Exp Annot)
	
makeMatchFunction sp pp xResult
 = do	
	-- make a guard for each of the patterns
	(vs, mGs)	<- liftM unzip $ mapM makeGuard pp
	let gs		= catMaybes mGs

	-- the new match expression has a single alternative, 
	--	with a new guard to match each of the argument patterns
	let xMatch	= case gs of
				[]	-> xResult
				_	-> D.XMatch sp Nothing [D.AAlt sp gs xResult]

	-- add the lambdas out the front
	let xFinal	= addLambdas sp vs xMatch

	return	$ xFinal
	
-- Make a new guard to match this pattern
makeGuard :: D.Pat a	-> RewriteM (Var, Maybe (D.Guard a))
makeGuard (D.WVar sp v)		
	= return (v, Nothing)

makeGuard pat			
 = do	v	<- newVarN NameValue
	let nn	= D.getAnnotW pat
	
 	return	(v, Just $ D.GExp nn pat (D.XVar nn v))
	
	
-- | Add some lambdas to the front of this expression
addLambdas 
	:: a	 			-- ^ source position to use on new nodes
	-> [Var]			-- ^ vars to bind with new lambdas
	-> D.Exp a 			-- ^ expressio to use as the body
	-> D.Exp a

addLambdas sp [] x	= x
addLambdas sp (v:vs) x	= D.XLambda sp v (addLambdas sp vs x)
 

-- Merge -------------------------------------------------------------------------------------------

-- | Merge consecutive pattern bindings into a single binding.
--	TODO: throw an error if merged bindings aren't consecutive
--	TODO: check for overlapping patterns		

--
mergeBindings
	:: [D.Stmt Annot] -> ([D.Stmt Annot], [S.Error])
	
mergeBindings ss	
 = let	( ss', errs)	= mergeBindings' [] [] ss
   in	( reverse ss'
   	, reverse errs)

mergeBindings' errAcc ssAcc []		= ([],	 		errAcc)
mergeBindings' errAcc ssAcc (s1:[])	= (s1 : ssAcc, 		errAcc)
mergeBindings' errAcc ssAcc
	(  s1@(SBind n1 (Just v1) x1)
	 : s2@(SBind n2 (Just v2) x2)
	 : ss)
	
	-- should be able to merge these
	| v1 == v2
	= let	-- break binding into lambda bound vars and body expression
		(vs1, xResult1)	= slurpLambdaVars x1
		(vs2, xResult2) = slurpLambdaVars x2
		
		-- do the merge
		(ssMerged, errs)= mergeBindings2
					n1
					s1 s2
					v1 vs1 xResult1 
			  		v2 vs2 xResult2
	   in	mergeBindings' (errs ++ errAcc) ssAcc (ssMerged ++ ss)
				
-- can't merge these
mergeBindings' errAcc ssAcc (s1 : s2 : ss)
	= mergeBindings' errAcc (s1 : ssAcc) (s2 : ss)
	

mergeBindings2 
	sp
	s1 s2
	v1 vs1 xResult1 
	v2 vs2 xResult2
	
	-- all bindings must have the same airity
	| length vs1 /= length vs2
	= dieWithUserError
		[S.ErrorBindingAirity v1 (length vs1) v2 (length vs2)]
	
	-- looks ok
	| otherwise
	= let	
		-- the two bindings have different names for their lambda bound variables.
		--	rewrite the second expression to have the same names as the first
		sub		= Map.fromList $ zip vs2 vs1
		xResult2_sub	= substituteVV sub xResult2

		-- merge the two expressions
		xMerged		= mergeMatchX xResult1 xResult2_sub
		xMerged_lam	= addLambdas sp vs1 xMerged

		sMerged		= SBind sp (Just v1) xMerged_lam
		
	  in  ([sMerged]
	      , [])

-- | Merge these two expressions into a single match
mergeMatchX 
	:: D.Exp Annot 
	-> D.Exp Annot 
	-> D.Exp Annot
		

-- two match expressions
mergeMatchX (XMatch n1 Nothing as1) (XMatch _ Nothing as2)
	= XMatch n1 Nothing (as1 ++ as2)

-- match and a default
mergeMatchX (XMatch n1 Nothing as1) x2
	= XMatch n1 Nothing
		(as1 ++ [AAlt n1 [] x2])

-- neither are a match expression, or only the second one is.
--	this is an obvious overlapped patterns problem, but Haskell
--	doesn't treat it as an error, so we won't either.
mergeMatchX x1 x2
 = let	n	= D.getAnnotX x1
   in  XMatch n Nothing
		[ AAlt n [] x1
		, AAlt n [] x2]


-- | Slurp the lambda bound variables from the front of this expression
slurpLambdaVars :: Exp a -> ([Var], Exp a)
slurpLambdaVars xx
 = case xx of
 	XLambda nn v x	
	 -> let	(moreVars, xFinal)	= slurpLambdaVars x
	    in	( v : moreVars
	    	, xFinal)
	
	_ 	-> ([], xx)






