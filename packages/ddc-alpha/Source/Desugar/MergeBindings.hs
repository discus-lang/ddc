
module Source.Desugar.MergeBindings 
	(mergeBindings)
where
import Source.Desugar.Base
import DDC.Var
import DDC.Desugar.Bits			as D
import DDC.Desugar.Exp			as D
import qualified DDC.Source.Error	as S
import qualified Data.Map		as Map


-- Merge -------------------------------------------------------------------------------------------
-- | Merge consecutive pattern bindings into a single binding, i.e:
--
--  f = \x y -> match \Delta_1
--  f = \a b -> match \Delta_2
--  
-- Is rewritten to:
--
--  f = \x y -> match \Delta_1, \Delta_2[a/x, b/y]
--
--	TODO: throw an error if merged bindings aren't consecutive
--	TODO: check for overlapping patterns		
--
mergeBindings
	:: [D.Stmt Annot] 
	-> ( [D.Stmt Annot]
           , [S.Error])
	
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
	
	-- all bindings must have the same airity.
	--	don't return anything for the first argument so mergeBindings' can progress.
	| length vs1 /= length vs2
	= ( []
	  , [S.ErrorBindingAirity v1 (length vs1) v2 (length vs2)] ) 
	
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


-- | Merge these two match expressions into a single match
mergeMatchX 
	:: D.Exp Annot -> D.Exp Annot	-- expressions to merge
	-> D.Exp Annot			-- result expression
		
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
 = let	Just n	= D.takeAnnotX x1
   in	XMatch n Nothing
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




