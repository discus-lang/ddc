{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Checking and reconstructing the type, effect and closure of an expression.
-- 
--   TODO: Check for out of scope vars.
--	   Check for fabricated witnesses.
--
module DDC.Core.Check.Exp
	( checkedTypeOfExp 
	, checkedTypeOfOpenExp
	, checkedEffectOfOpenExp
	, checkOpenExp
	, checkExp
	, checkExp')	-- used by DDC.Core.Lint.Prim
where
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Core.Glob
import DDC.Core.Exp
import DDC.Core.Check.Base
import DDC.Core.Check.Env
import DDC.Core.Check.Prim
import DDC.Core.Check.Type
import DDC.Base.Literal
import DDC.Base.DataFormat
import DDC.Util.FreeVars
import DDC.Type
import DDC.Var
import Data.Maybe
import DDC.Type.EffectStore		(EffectStore)
import DDC.Type.ClosureStore		(ClosureStore)
import qualified DDC.Type.EffectStore	as Eff
import qualified DDC.Type.ClosureStore	as Clo
import qualified Shared.VarUtil		as Var
import qualified Data.Map		as Map
import qualified Data.Set		as Set

stage	= "DDC.Core.Check.Exp"

-- Wrappers ---------------------------------------------------------------------------------------
-- | Check an expression, returning it's type.
--	The expression must be closed. No free variables.
checkedTypeOfExp :: String -> Exp -> Type
checkedTypeOfExp callerName xx
 = let 	env		= envInit callerName globEmpty globEmpty
  	(_, t, _, _)	= checkExp xx env 
   in	t


-- | Check an expression, returning it's type.
--	Variables can be free provided they are annotated with their types.
checkedTypeOfOpenExp :: String -> Exp -> Type
checkedTypeOfOpenExp callerName xx
 = let	env		= (envInit callerName globEmpty globEmpty)
				{ envClosed	= False }
	(_, t, _, _)	= checkExp xx env
   in	t


-- | Check an expression, returning it's effect.
--	Variables can be free provided they are annotated with their types.
checkedEffectOfOpenExp :: String -> Exp -> Effect
checkedEffectOfOpenExp callerName xx
 = let	env		= (envInit callerName globEmpty globEmpty)
				{ envClosed	= False }
	(_, _, eff, _)	= checkExp xx env
   in	eff


-- | Check an expression.
--	Variables can be free provided they are annotated with their types.
checkOpenExp :: Exp -> Env -> (Exp, Type, Effect, Closure)
checkOpenExp xx env
 = let	env'		= env { envClosed = False }
   in	checkExp xx env'


-- Exp --------------------------------------------------------------------------------------------
-- | Check an expression.
checkExp :: Exp -> Env -> (Exp, Type, Effect, Closure)
checkExp xx env
 = let	!(xx', t, effs, clos)	= checkExp' 0 xx env
   in	( xx'
	, t
	, Eff.toEffect  effs
	, Clo.toClosure clos )
	
-- | Check an expression. Also takes an indent level for tracing.
checkExp' :: Int -> Exp -> Env -> (Exp, Type, EffectStore, ClosureStore)
checkExp' n xx env
 = if debugExp
    then let result@(xx', t, eff, clo)	
		= trace (pprAtColumn (n*indenting) $ vcat
			[ ppr (replicate 70 '-') %% "Exp" %% n
			, ppr xx ])
		$ checkExp_trace n xx env

	 in trace 
		(pprAtColumn (n*indenting) $ vcat 
		[ "type:   " 	% t
		, "effect: "	% eff
		, "closure:\n"	%> ppr clo
		, ppr xx'
		, "--" %% "Exp" %% n %% ppr (replicate 70 '-')])
		result

   else	checkExp_trace n xx env

checkExp_trace m xx env
 = let n	= m + 1
   in case xx of
	XNil	
	 -> panic stage "checkExp: XNil"
	
	-- Variables
	-- If the type isn't attached directly to the var then try to look
	-- it up from the type environment.
	XVar v TNil
	 | Just t	<- typeFromEnv v env
	 -> checkExp_trace m (XVar v t) env
	
	 | otherwise
	 -> panic stage $ vcat
		[ "checkExp: Can't find type for variable" % v
		, ppr "    There is no annotation, and the variable is not in the environment."
		, "During: "	% envCaller env]

	-- TODO: Check type annotation matches the one from the environment.
	XVar v t1
{-	 | envClosed env
	 , Nothing	<- typeFromEnv v env
	 -> panic stage $ vcat
		[ "checkExp: Variable " % v 
			% " is out of scope when checking closed expression."
		, "During: "	% envCaller env]
-}	
	 | varNameSpace v /= NameValue
	 -> panic stage 
		$ "checkExp: invalid namespace for variable." 
		% v %% (show $ varNameSpace v)
	
	 | otherwise
	 , (t1', k)	<- checkTypeI n t1 env 
	 -> k `seq`
	    let	-- Running the trimmer also produces tDanger terms,
		-- but we don't need those in the core language, only in source.
{-		crushDanger c
			| Just (v', t')	<- takeTFree c
			, Just (_,  t2)	<- takeTDanger t'
			= makeTFreeBot v' t2
			
			| otherwise
			= c
-}		
		-- Crush out all the tDanger terms.
		clo	= makeTSum kClosure 
--			$ map crushDanger 
			$ flattenTSum
			$ trimClosureNoDangerC
			$ makeTFreeBot v
			$ t1		  

		-- TODO: We're ignoring closure terms due to constructors of arity zero,
		--	 like True :: Bool %r1. Prob want to handle this in a more generic way,
		--	 like masking closure terms involving constant regions.
		clos 	| Var.isCtorName v	= Clo.empty
			| otherwise		= Clo.fromClosure clo
			
	   in	( XVar v t1'
		, t1'
		, Eff.pure
		, clos)


	-- Literal values.
	-- HACK: We handle applications of string literals to their regions directly
	--       so we don't have to come up with its actual type scheme. Doing this
	--       would require a unique region variable...
	XAPP (XLit (LiteralFmt LString{} Unboxed)) (TVar k r)
	 | k == kRegion
	 , Just tc	<- tcString Unboxed
	 ->	( xx
		, TApp (TCon tc) (TVar kRegion r)
		, Eff.pure
		, Clo.empty)

	XLit litFmt
	 | Just tcLit	<- tyConOfLiteralFmt litFmt
	 -> 	( xx
		, TCon tcLit
		, Eff.pure
		, Clo.empty)


	-- Type abstraction.
	XLAM BNil k x
	 | (k', w)		<- checkKindI n k env 
	 , (x', t, eff, clo)	<- checkExp' n x env
	 -> w `seq` 
		( XLAM BNil k' x'
		, TForall BNil k t
		, eff
		, clo )
	
	XLAM (BVar v) k x
	 | (k', w)		<- checkKindI n k env
	 , (x', t, eff, clo)	<- withKindBound v k' Nothing env 
				$! checkExp' n x
	 -> w `seq`
		( XLAM (BVar v) k' x'
		, TForall (BVar v) k t
		, eff
		, clo)
	
	XLAM (BMore v t3) k x
	 | (k',  w)		<- checkKindI n k env
	 , (t3', k3)		<- checkTypeI n t3 env
	 , isEquiv $ equivKK k k3
	 , (x', t, eff, clo)	<- withKindBound v k (Just t3') env
				$! checkExp' n x
	 -> w `seq`
		( XLAM (BMore v t3') k' x'
		, TForall (BMore v t3') k t
		, eff
		, clo)
		
		
	-- Type application.
	-- TODO: Handle subsitutions on effect\/closure stores more cleverly.
	XAPP x1 t2
	 | (x1', t1, eff, clo)	<- checkExp'  n x1 env
	 , (t2', k2)		<- checkTypeI n t2 env
	 , k2'			<- crushK k2
	 -> case t1 of
		TForall BNil k11 t12
		 | isEquiv $ equivKK (crushK k11) k2'
		 -> 	( XAPP x1' t2'
			, t12
			, eff
			, clo)
		
		TForall (BVar v) k11 t12
		 | isEquiv $ equivKK (crushK k11) k2'
		 , sub	<- subVT_everywhere (Map.singleton v t2')
		 -> 	( XAPP x1' t2'
		    	, sub t12
		    	, Eff.fromEffect  $ sub $ Eff.toEffect  eff
		    	, Clo.fromClosure $ sub $ Clo.toClosure clo)
		
		-- TODO: check against the more-than constraint
		TForall (BMore v _) k11 t12
		 | isEquiv $ equivKK (crushK k11) k2'
		 , sub	<- subVT_everywhere (Map.singleton v t2')
		 -> 	( XAPP x1' t2'
			, sub t12
		    	, Eff.fromEffect  $ sub $ Eff.toEffect  eff
		    	, Clo.fromClosure $ sub $ Clo.toClosure clo)
		
		_ -> panic stage $ vcat
			[ ppr "Type mismatch in (value/type) application."
			, "During:\n"		 %> envCaller env, blank
			, "Cannot apply type:\n" %> t2,  blank
			, "     of kind:\n"	 %> k2', blank
			, "to expression:\n" 	 %> xx,	 blank
			, "with type:\n"	 %> t1,	 blank]
			

	-- Value abstraction
	-- TODO: check annots have correct kind
	XLam v1 t1 x2 effAnn cloAnn
	 | varNameSpace v1 /= NameValue
	 -> panic stage
		$ "checkExp invalid namespace for variable "
		% v1 %% (show $ varNameSpace v1)
		
	 | otherwise
	 , (t1', k1)			<- checkTypeI n t1 env
	 , (x2', t2, eff2, clo2)	<- withType v1 t1 env $! checkExp' n x2
	 , (effAnn', kEffAnn)		<- checkTypeI n effAnn env
	 , (cloAnn', kCloAnn)		<- checkTypeI n cloAnn env
 	 -> k1 `seq` kEffAnn `seq` kCloAnn `seq`
	    let	
		-- Mask closure terms due to the bound variable.
		!clo2_masked	= Clo.mask v1 clo2

		-- Check the reconsructed closure is subsumed by the annotation.
		!cloSubs	= subsumesTT (Clo.toClosure clo2_masked) cloAnn'

		-- Get the set of visible region variables.
		-- These are vars that are present in the parameter or return type, 
		-- or in one of the types of the closure.
		vsVisible	
			= Set.filter (\v -> varNameSpace v == NameRegion)
			$ Set.unions
				[ freeVars t1
				, freeVars t2
				, freeVars $ Clo.toClosure clo2_masked ]

		-- Mask effects on regions that are not visible to callers.
		!eff2_masked	= Eff.maskReadWritesNotOn vsVisible eff2

		-- Check the reconstructed effect is subsumed by the annotation.
		!effAnn_crushed	= crushT $ effAnn'
		!effSubs	= subsumesTT (Eff.toEffect eff2_masked) effAnn_crushed
		
		result
		 | not $ isSubsumes effSubs 
		 = panic stage $ vcat
			[ ppr "Effect mismatch in lambda abstraction."
			, "During:\n"				%> envCaller env, blank
			, "Effect of body:\n" 			%> eff2,	blank
			, "with closure:\n"			%> clo2_masked,	blank
			, "visible vars:\n"			%> vsVisible,	blank
			, "masked effect:\n"			%> eff2_masked,	blank
			, "does not match effect annotation:\n"	%> effAnn_crushed, blank
			, "in expression:\n"			%> xx,		blank]
		
		 | not $ isSubsumes cloSubs
		 = panic stage $ vcat
			[ ppr "Closure mismatch in lambda abstraction."
			, "During:\n"		 		%> envCaller env, blank
			, "Closure of abstraction:\n"	 	%> clo2_masked,	blank
			, "is not less than annotation:\n" 	%> show cloAnn,	blank
			, "in expression:\n"			%> xx,		blank]

		 -- We've just shown that the reconstructed effect and closures are subsumed by
		 -- those on the annots. The annots are sometimes larger than needed due to
		 -- bi-directional unification in the type inferencer. Better to replace them
		 -- with the reconstructed ones.
		 | otherwise
		 = 	( XLam v1  t1' x2' (Eff.toEffect eff2_masked) (Clo.toClosure clo2_masked)
			, makeTFun t1' t2  (Eff.toEffect eff2_masked) (Clo.toClosure clo2_masked)
		        , Eff.pure
		        , clo2_masked)
	  in result

	-- Value application
	-- TODO: why is it ok to discard the third closure?
	XApp x1 x2
	 | (x1', t1, eff1, clo1) <- checkExp' n x1 env
	 , (x2', t2, eff2, clo2) <- checkExp' n x2 env
	 , t1'			 <- crushT $ trimClosureNoDangerT t1
	 , t2'			 <- crushT $ trimClosureNoDangerT t2
	 -> case takeTFun t1' of
	     Just (t11, t12, eff3, _)
	      -> case subsumesTT t2' t11 of
		  Subsumes 
		   -> ( XApp x1' x2'
		      , t12
		      , Eff.unions [eff1, eff2, Eff.fromEffect eff3]
		      , Clo.union  clo1 clo2)
		
		  NoSubsumes s1 s2
		   -> panic stage $ vcat
			[ ppr "Type error in application."
			, "During:\n"		 		%> envCaller env, blank
			, "  cannot apply function of type: " 	% t1',	blank
			, "            to argument of type: " 	% t2',	blank
			, ppr "   because",				blank
			, "                           type: " 	% s2,	blank
			, "               does not subsume: " 	% s1,	blank
			, blank
			, "in application:\n" % xx]
		
	     _ -> panic stage $ vcat
			[ ppr "Type error in application."
			, "During:\n"		 		%> envCaller env, blank
			, "  cannot apply non-function type: " 	% t1',	blank
			, "             to argument of type: "	% t2',	blank
			, blank
			, " in application: " % xx]

	-- Do expression.
	XDo ss
	 | (ss', tLast, eff, clo)	<- checkStmts n ss env
	 -> 	( XDo ss'
		, tLast
		, eff
		, clo)

	-- Match expression.
	XMatch aa
	 | (aa', tResult, eff, clo)	<- checkAlts n aa env
	 ->	( XMatch aa'
	 	, tResult
		, eff
		, clo)

	-- Local region binding.
	-- TODO: check r not free in type.
	XLocal vRegion vtsWit xBody
	 -> let	
		-- The region variable is in-scope in the witnesses expressions.
		checkWitness t
			= withKindBound vRegion kRegion Nothing env
			$ checkTypeI n t

		-- Check all the witness expressions.
		vtksWit	= map (\(v, t)      -> (v, checkWitness t)) vtsWit
		vtsWit'	= map (\(v, (t, _)) -> (v, t))		   vtksWit
		vksWit	= map (\(v, (_, k)) -> (v, k, Nothing))    vtksWit
		
		vksAll	= (vRegion, kRegion, Nothing) : vksWit
		
		-- Check the body.
		-- Kinds for the region and witnesses are added to the environment.
		(xBody', tBody, eff, clo)
			= withKindBounds vksAll env
			$ checkExp' n xBody
		
		-- Mask effects on the bound region variable.
		eff_masked	= Eff.maskReadWritesOn vRegion eff
	
	    in	( XLocal vRegion vtsWit' xBody'
		, tBody
		, eff_masked
		, clo)

	-- Primitive operator.
	XPrim  prim xs	
	 | (xs', t1, eff, clo)	<- checkPrim n prim xs env
	 ->	( XPrim prim xs'
		, t1
		, eff
		, clo)

	-- Type annotation
	XTau tAnnot x
	 | (tAnnot', kAnnot)	<- checkTypeI n tAnnot env
	 , (x', tExp, eff, clo)	<- checkExp' n x env
	 , tExp'		<- trimClosureNoDangerT $ crushT tExp
	 -> kAnnot `seq`
	    if isSubsumes $ subsumesTT tExp' tAnnot'
		then	( XTau tAnnot' x'
			, tExp'
			, eff
			, clo)
			
		else panic stage $ vcat
			[ ppr "Type error in type annotation.",			blank
			, "During:\n"		 		%> envCaller env, blank
			, "  Reconstructed type:\n"		%> tExp',	blank
			, "  does not match annotation:\n"	%> tAnnot,	blank
			, "  on expression:\n"			%> xx]
		
	_ -> panic stage 
		$ vcat 	[ ppr "checkExp: no match for:"
		  	, ppr xx]
		

-- Statements -------------------------------------------------------------------------------------
-- | Check a list of (possibly recursive) statements.
checkStmts 
	:: Int -> [Stmt] -> Env 
	-> ([Stmt], Type, EffectStore, ClosureStore)

-- TODO: this doesn't handle recursive statements.
checkStmts n ss env
 = let	(ss', t, eff, clo)	
		= checkStmts' n env ss [] Eff.pure Clo.empty

	-- Delete closure terms due to variables bound by the statements.
	vsBound	= [v | SBind (Just v) _ <- ss' ]
  	clo'	= foldr Clo.mask clo vsBound

   in	(ss', t, eff, clo')

checkStmts' _ _ [] _ _ _
 	= panic stage
	$ "checkStmts': no statements"

-- Return the type of the last one.	
checkStmts' n env (SBind b x : []) ssAcc effAcc cloAcc
 = let	(x', t, eff, clo)	= checkExp' (n+1) x env
   in	( reverse (SBind b x' : ssAcc)
	, t
	, Eff.union eff effAcc
	, Clo.union clo cloAcc)
	
checkStmts' n env (SBind Nothing x : ss) ssAcc effAcc cloAcc 
 = let	(x', t, eff, clo)	= checkExp' n x env
   in 	t `seq`
	checkStmts' n env ss 
		(SBind Nothing x' : ssAcc)
		(Eff.union eff effAcc)
		(Clo.union clo cloAcc)

checkStmts' n env (SBind (Just v) x : ss) ssAcc effAcc cloAcc
 = let	(x', t, eff, clo)	= checkExp' n x env
   in	t `seq`
	withType v t env $ \env' -> 
	 checkStmts' n env' ss 
		(SBind (Just v) x' : ssAcc)
		(Eff.union eff effAcc)
		(Clo.union clo cloAcc)


-- Alternatives -----------------------------------------------------------------------------------
-- | Check a list of match alternatives.
checkAlts 
	:: Int -> [Alt] -> Env
	-> ([Alt], Type, EffectStore, ClosureStore)

checkAlts n as env
	= checkAlts' n env as [] [] Eff.pure Clo.empty

checkAlts' _ _ [] types altAcc effAcc cloAcc
 = 	( reverse altAcc
	, fromMaybe 
		(panic stage $ "checkAlts: can't join types")
		(joinSumTs types)
	, effAcc
	, cloAcc)

checkAlts' n env (AAlt gs x : as) types altAcc effAcc cloAcc
 = let	(gs', x', tBody, effGuards, cloGuards)
		= checkGuards n gs x env
	
   in	checkAlts' n env as 
		(tBody : types)
		(AAlt gs' x' : altAcc)
		(Eff.union effGuards effAcc)
		(Clo.union cloGuards cloAcc)


-- Guards -----------------------------------------------------------------------------------------
-- | Check some pattern guards.
--   The variables bound by the pattern are in scope in successive guards.
checkGuards
	:: Int -> [Guard] -> Exp -> Env
	-> ([Guard], Exp, Type, EffectStore, ClosureStore)

checkGuards n [] xBody env
 = let	(xBody', t, eff, clo)	= checkExp' (n+1) xBody env
   in	( []
	, xBody'
	, t
	, eff
	, clo)
	
checkGuards n (GExp (WVar v) x : gsRest) xBody env
 = let	
	-- Check the right of the guard.
	(x', tExp, effExp, cloExp)	
		= checkExp' (n+1) x env

	-- Check the rest of the guards and body.
	(gsRest', xBody', tRest, effRest, cloRest)
		= withType v tExp env
		$ checkGuards n gsRest xBody
	
	-- The effect of the guards and body.
	eff'	= Eff.union effExp effRest
	
	-- Delete the closure terms due the var bound here.
	clo'	= Clo.mask v $ Clo.union cloExp cloRest
		
   in	( GExp (WVar v) x' : gsRest'
	, xBody'
	, tRest
	, eff'
	, clo')


-- TODO: Check the the expression has the type of the literal.
checkGuards n (GExp (WLit sp lf) x : gsRest) xBody env
 = let	
	-- Check the right of the guard.
	(x', tExp, effExp, cloExp)
		= checkExp' (n+1) x env

	-- Check the rest of the guards and body.
	(gsRest', xBody', tRest, effRest, cloRest)
		= checkGuards n gsRest xBody env

	-- The effect of the whole group of guards includes the effect
	-- of inspecting the case object to see if it matches.
	effMatch = Eff.fromEffect $ effectOfMatchAgainst tExp
	eff'	 = Eff.unions [effExp, effMatch, effRest]

	-- The closure of the guards and body.
	clo'	= Clo.union cloExp cloRest
	
   in	( GExp (WLit sp lf) x' : gsRest'
	, xBody'
	, tRest
	, eff'
	, clo')


-- TODO: check against type of the pattern.
checkGuards n (GExp (WCon sp vCon lvts) x : gsRest) xBody env
 = let	
	-- Check the right of the guard.
	(x', tExp, effExp, cloExp)	
		= checkExp' (n+1) x env

	-- Add vars bound by the pattern to the environment.
	vts	= Map.fromList $ [(v, t) | (_, v, t) <- lvts]
	env'	= env { envTypes = Map.union (envTypes env) vts }

	-- Check the rest of the guards and body.
	(gsRest', xBody', tRest, effRest, cloRest)
		= checkGuards n gsRest xBody env'

	-- The effect of the whole group of guards includes the effect
	-- of inspecting the case object to see if it matches.
	effMatch = Eff.fromEffect $ effectOfMatchAgainst tExp
	eff'	 = Eff.unions [effExp, effMatch, effRest]

	-- Delete the closure terms due to vars bound by the pattern.
	vsBound	= [v | (_, v, _) <- lvts]
	clo	= Clo.union cloExp cloRest
	clo'	= foldr Clo.mask clo vsBound

   in	( GExp (WCon sp vCon lvts) x' : gsRest'
	, xBody'
	, tRest
	, eff'
	, clo')


-- | Get the effect resulting from a pattern match against a value of this type.
effectOfMatchAgainst :: Type -> Effect
effectOfMatchAgainst tt
 = case takeTData tt of
	Just (_, _, (tR@(TVar kR _) : _))
	 | isRegionKind kR
	 -> TApp tRead tR
	
	Just (_, _, [])
	  -> tPure
	
	_ 	-> panic stage
		$  "effectOfMatchAgainst: " % tt

