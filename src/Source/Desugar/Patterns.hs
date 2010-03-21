module Source.Desugar.Patterns
	( rewritePatVar
	, rewritePatternsTreeM
	, makeMatchFunction
	, makeMatchExp
	, makeGuard)
where
import Util
import Shared.VarPrim
import Shared.Base
import Source.Desugar.Base
import Source.Desugar.MergeBindings
import Shared.Literal
import Shared.Var			(Var, NameSpace(..))
import Desugar.Exp			as D
import Desugar.Bits			as D
import Desugar.Plate.Trans		as D
import qualified Shared.Var		as Var


-- rewritePatTree ----------------------------------------------------------------------------------
rewritePatternsTreeM
	:: D.Tree Annot 
	-> RewriteM (D.Tree Annot)

rewritePatternsTreeM tree
 = do	
 	-- expand out patterns in match expressions
	tree'	<- mapM (D.transformXM rewritePatX) tree

	-- merge bindings at top level
	let (psBind, psRest)	= partition (=@= PBind{}) tree'
	let (ssMerged, errs)	= mergeBindings $ map topBindToStmt psBind
	let psMerged		= map stmtToTopBind ssMerged
	mapM_ addError errs
			
	return	$ psRest ++ psMerged

topBindToStmt (PBind n mV x)	= SBind n mV x
stmtToTopBind (SBind n mV x)	= PBind n mV x

rewritePatX :: D.Exp Annot -> RewriteM (D.Exp Annot)
rewritePatX xx
 = case xx of
 	D.XMatch nn co aa
	 -> do	aa'	<- mapM (rewritePatA co) aa
	 	return	$ D.XMatch nn co aa'

	D.XDo nn ss
	 -> do	let (ss', errs)	= mergeBindings ss
		mapM_ addError errs
		
	   	return	$ D.XDo nn ss'

	_ -> return xx
		
rewritePatA co aa
 = case aa of
	D.AAlt sp gs x
	 -> do	gsAts		<- mapM (sprinkleAtsG sp) gs
	  	let gsLift	= catMap simplifyGuard gsAts
	  	gsDesugared <- catMapM desugarLiteralGuard gsLift
		return	$ D.AAlt sp gsDesugared x


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
		
	D.WLit nn l
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
	
	D.WLit nn l
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
	


-- | Desugar guards against boxed literals into calls of (possibly user-defined) (==).
desugarLiteralGuard :: D.Guard Annot -> RewriteM [D.Guard Annot]
desugarLiteralGuard g
 = case g of
	D.GExp le (WLit ll fmt@(LiteralFmt _ df)) e
	 | dataFormatIsBoxed df
	 -> do  let equals = D.XVar le primEq
		let true = D.WConLabel le primTrue []
		let ($$) = D.XApp le
		return [D.GExp le true (equals $$ D.XLit ll fmt $$ e)]
	
	D.GCase le (WLit ll fmt@(LiteralFmt _ df))
	 | dataFormatIsBoxed df
	 -> do	exps <- litToExp ll fmt
		catMapM desugarLiteralGuard exps

	_ -> return [g]

-- | Rewrite a literal in a 'case' as an expression.
--
-- eg	match foo with {
--	  || "hello"
--
-- =>	match foo with {
--	  || v1
--	  , "hello" <- v1
--
-- desugarLiteralGuard then finishes the job.
--
-- =>	match foo with {
--	  || v1
--	  , True <- "hello" == v1
litToExp :: Annot -> LiteralFmt -> RewriteM [D.Guard Annot]
litToExp ll fmt
 = do	val <- newVarN NameValue
	return [D.GCase ll (D.WVar ll val),
	        D.GExp ll (D.WLit ll fmt) (D.XVar ll val)]

-- makeMatchFunction -------------------------------------------------------------------------------

-- | Build a function that matches on the given pattern expressions then returns the result
--
-- eg 	makeMatchFunction  [(x, y) : xs, Just 3] exp
--
-- => 	\v1 v2  
--	  -> match { 
--		| (v1, y) : xs 	<- v1 
--		| Just 3	<- v2
--		= exp
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
	-- make the body expression
	(vsFree, xBody)	<- makeMatchExp sp pp xResult

	-- add the lambdas out the front
	let xFinal	= addLambdas sp vsFree xBody

	return	$ xFinal
	
	
makeMatchExp
	:: SourcePos 
	-> [D.Pat Annot]
	-> D.Exp Annot
	-> RewriteM ([Var], D.Exp Annot)
	
makeMatchExp sp pp xResult
 = do
	-- make a guard for each of the patterns
	(vs, mGs)	<- liftM unzip $ mapM makeGuard pp
	let gs		= catMaybes mGs

	-- the new match expression has a single alternative, 
	--	with a new guard to match each of the argument patterns
	let xMatch	= case gs of
				[]	-> xResult
				_	-> D.XMatch sp Nothing [D.AAlt sp gs xResult]
	return (vs, xMatch)

	
-- Make a new guard to match this pattern
makeGuard 
	:: D.Pat a	
	-> RewriteM 
		( Var
		, Maybe (D.Guard a))

makeGuard (D.WVar sp v)		
	= return (v, Nothing)

makeGuard pat			
 = do	v	<- newVarN NameValue
	let nn	= D.getAnnotW pat
	
 	return	(v, Just $ D.GExp nn pat (D.XVar nn v))


	
