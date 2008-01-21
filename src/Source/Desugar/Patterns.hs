
module Source.Desugar.Patterns
	( rewritePatVar
	, rewritePatTreeM
	, makeMatchFunction 
	, expToPat )

where

import Util

import qualified Shared.Var	as Var
import qualified Shared.VarUtil	as Var

import Shared.Var	(Var, NameSpace(..))
import Shared.VarPrim
import Shared.Base
import Shared.Error

import qualified Source.Exp		as S
import qualified Source.Util		as S

import qualified Desugar.Exp		as D
import qualified Desugar.Bits		as D
import qualified Desugar.Plate.Trans	as D

import Source.Desugar.Base

-----
stage	= "Source.Desugar.Patterns"


-- rewritePatTree ----------------------------------------------------------------------------------

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
	  	let gsLift	= catMap simplifyGuard gsAts
		return	$ D.AAlt none gsLift x


-- SprinkleAts -------------------------------------------------------------------------------------

-- SprinkleAts ensures that every intermediate node in a pattern is named with an XAt variable.
--	This makes crushing and slurping pattern much easier because we don't have to
--	name the nodes on the fly.

-- | Name intermediate nodes in a pattern expression
sprinkleAtsX :: SourcePos -> S.Exp -> RewriteM S.Exp
sprinkleAtsX sp p
 = case p of
 	S.XVar sp var
	 | Var.isCtorName var
	 -> do	v	<- newVarN NameValue
	 	return	$ S.XAt sp v (S.XCon sp var [])
		
	 | otherwise
	 ->	return	$ S.XVar sp var
 
 	S.XCons sp x1 x2
	 -> do	v	<- newVarN NameValue
	 	x1'	<- sprinkleAtsX sp x1
		x2'	<- sprinkleAtsX sp x2
		return	$ S.XAt sp v (S.XCons sp x1' x2')
 
 	S.XTuple sp xx
	 -> do	v	<- newVarN NameValue
	 	xx'	<- mapM (sprinkleAtsX sp) xx
		return	$ S.XAt sp v (S.XTuple sp xx')
		
	-- convert from defixed pattern into XCon form.
	S.XApp{} 
	 -> do	v	<- newVarN NameValue
	 	let (S.XVar sp var : xs)	= S.flattenApps p
	 	xs'		<- mapM (sprinkleAtsX sp) xs
		let var'	=  rewritePatVar var
		return	$ S.XAt sp v (S.XCon sp var' xs')
			

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

-- Name intermediate nodes in this pattern
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
	

-- simplifyGuard -----------------------------------------------------------------------------------

-- A list of vars and what pattern to match them against
type CollectAtS 	= [(Var, D.Pat Annot)]
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
		: [ D.GExp none p (D.XVar none v) 	-- build a match for each internal node
				| (v, p) <- vws]

  in	gs

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
 


-- | Rewrite a source pattern expression to an actual pattern
--	Patterns in the arguments to function bindings are initially parsed as
--	expressions due to limiations with the LR(1) parser.
--
--	This function converts these expressions to actual patterns.
--
expToPat :: S.Exp -> S.Pat
expToPat xx
 = case xx of
	S.XVar sp v
	 | Var.isCtorName v
	 -> S.WCon v []

	 | otherwise
	 -> S.WVar v

 	S.XTuple sp xs	-> S.WTuple (map expToPat xs)

	S.XApp sp x1 x2
	 -> let	(S.XVar sp v : xs)	= S.flattenApps xx
	    in	S.WCon v (map expToPat xs)
		
	S.XList sp xx	-> S.WList (map expToPat xx)	

	S.XConst sp c	-> S.WConst c
	
	_	-> panic stage
		$ "expToPat: no match for " % show xx % "\n"

 
{-
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
-}


