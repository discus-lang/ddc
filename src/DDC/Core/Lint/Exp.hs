{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Core.Lint.Exp
	( checkedTypeOfExp 
	, checkExp
	, checkExp')	-- used by DDC.Core.Lint.Prim
where
import Core.Util.Substitute
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Core.Glob
import DDC.Core.Exp
import DDC.Core.Lint.Base
import DDC.Core.Lint.Env
import DDC.Core.Lint.Prim
import DDC.Core.Lint.Type
import DDC.Base.Literal
import DDC.Base.DataFormat
import DDC.Util.FreeVars
import DDC.Type
import DDC.Var
import Data.Maybe
import DDC.Type.ClosureStore		(ClosureStore)
import Data.Sequence			(Seq)
import qualified DDC.Type.ClosureStore	as Clo
import qualified Shared.VarUtil		as Var
import qualified Data.Sequence		as Seq
import qualified Data.Set		as Set
import qualified Data.Foldable		as Foldable

stage	= "DDC.Core.Lint.Exp"

-- Wrappers ---------------------------------------------------------------------------------------
checkedTypeOfExp :: String -> Exp -> Type
checkedTypeOfExp callerName xx
 = case checkExp xx (envInit callerName globEmpty globEmpty) of
	(_, t, _, _)	-> t
	


-- Exp --------------------------------------------------------------------------------------------
-- | Check an expression, returning its type.
--   TODO: Also make this optionally annotate vars with their types.
checkExp 
	:: Exp -> Env 
	-> (Exp, Type, Effect, Closure)

checkExp xx env
 = let	!(xx', t, effs, clos)	= checkExp' 0 xx env
   in	( xx'
	, t
	, makeTSum kEffect $ Foldable.toList effs
	, Clo.toClosure clos )
	
checkExp' 
	:: Int 			-- ^ Indent level for tracing.
	-> Exp			-- ^ Expression to check.
	-> Env			-- ^ Type environment and configuration.
	-> ( Exp		--  Checked expression.
	   , Type		--  Type of expression.
	   , Seq Effect		--  Effect of expression.
	   , ClosureStore)	--  Closure of expression.

checkExp' n xx env
 = if debugExp
    then let result@(xx', t, eff, clo)	
		= trace (setColumn (n*indenting) % vcat
			[ ppr (replicate 70 '-') <> "Exp" <> n
			, ppr xx' ])
		$ checkExp_trace n xx env

	 in trace 
		(setColumn (n*indenting) % vcat 
		[ "type:   " 	% t
		, "effect: "	% eff
		, "closure:\n"	%> ppr clo
		, ppr xx
		, "--" <> "Exp" <> n <> ppr (replicate 70 '-')])
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
	 -> panic stage
		$ "checkExp: var " % v 
		% " is not annotated with its type, and it's not in the environment"

	XVar v t1
	 | varNameSpace v /= NameValue
	 -> panic stage 
		$ "checkExp: invalid namespace for variable " 
		% v <> (show $ varNameSpace v)
	
	 -- TODO: Merge the trimming junk with the ClosureStore insert operator.
	 | otherwise
	 , (t1', k)	<- checkTypeI n t1 env 
	 -> k `seq`
	    let	!clo	= trimClosureC_constrainForm
			$ makeTFreeBot v 
			$ toConstrainFormT t1
			
		crushDanger c
			| Just (v', t')	<- takeTFree c
			, Just (_,  t2)	<- takeTDanger t'
			= makeTFreeBot v' t2
			
			| otherwise
			= c

		clo_dump
			= packType
			$ makeTSum kClosure 
			$ map crushDanger 
			$ flattenTSum clo		  

		-- TODO: we're ignoring closure terms due to constructors
		--       of arity zero, like True :: Bool %r1.
		--       Is is good enough to say this is ok because the region is always constant?
		clo' 	| isTBot clo_dump	= Clo.empty
			| Var.isCtorName v	= Clo.empty
			
			-- If the trimmer closure just has a single part then add that.
			| otherwise
			= Clo.fromClosure clo_dump
			
	   in	( XVar v t1'
		, t1'
		, Seq.singleton tPure
		, clo')


	-- Literal values
	-- HACK: Handle applications of string literals to their regions directly
	--       so we don't have to come up with its actual type scheme. Doing this
	--       would require a fresh region variable...
	XAPP (XLit (LiteralFmt LString{} Unboxed)) (TVar k r)
	 | k == kRegion
	 , Just tc	<- tcString Unboxed
	 ->	( xx
		, TApp (TCon tc) (TVar kRegion r)
		, Seq.empty
		, Clo.empty)

	XLit litFmt
	 | Just tcLit	<- tyConOfLiteralFmt litFmt
	 -> 	( xx
		, TCon tcLit
		, Seq.singleton tPure
		, Clo.empty)


	-- Type abstraction
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
		
		
	-- Type application
	-- TODO: BAD! don't use substitute here. 
	--       better to propagate a list of constraints back up the tree.
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
		 , sub 	<- substituteT (subSingleton v t2')
		 -> 	( XAPP x1' t2'
		    	, sub t12
		    	, fmap sub eff
		    	, Clo.fromClosure $ sub $ Clo.toClosure clo)
		
		-- TODO: check against more-than constraint
		TForall (BMore v _) k11 t12
		 | isEquiv $ equivKK (crushK k11) k2'
		 , sub	<- substituteT (subSingleton v t2')
		 -> 	( XAPP x1' t2'
			, sub t12
		    	, fmap sub eff
		    	, Clo.fromClosure $ sub $ Clo.toClosure clo)
		
		_ -> panic stage $ vcat
			[ ppr "Type mismatch in (value/type) application."
			, "During:\n"		 %> envCaller env, blank
			, "Cannot apply type:\n" %> t2,  blank
			, "of kind:\n"		 %> k2', blank
			, "to expression:\n" 	 %> xx,	 blank
			, "with type:\n"	 %> t1,	 blank]
			

	-- Value abstraction
	-- TODO: Shift the masking code to a different module.
	-- TODO: check annots have correct kind
	XLam v1 t1 x2 effAnn cloAnn
	 | varNameSpace v1 /= NameValue
	 -> panic stage
		$ "checkExp invalid namespace for variable "
		% v1 <> (show $ varNameSpace v1)
		
	 | otherwise
	 , (t1', k1)			<- checkTypeI n t1 env
	 , (x2', t2, eff2, clo2)	<- withType v1 t1 env $! checkExp' n x2
	 , (effAnn', kEffAnn)		<- checkTypeI n effAnn env
	 , (cloAnn', kCloAnn)		<- checkTypeI n cloAnn env
 	 -> k1 `seq` kEffAnn `seq` kCloAnn `seq`
	    let	-- The closure annotation on the abstraction is the closure of the body
		-- minus the variable that is bound at this point.
		!clo2_cut	= Clo.mask v1 clo2

	 	-- TODO: having to do the conversion here is really slow.
		-- NOTE: In higher order cases the annotation can be a closure variable, 
		--       with or without a bound, while the reconstructed closure is always
		--       a set of TFrees.
		!cloSubs	= subsumesTT (Clo.toClosure clo2_cut) cloAnn

		-- The visible region variables.
		-- These are vars that are present in the parameter or return type, 
		-- or in one of the types in the closure.
		vsVisible	
			= Set.filter (\v -> varNameSpace v == NameRegion)
			$ Set.unions
				[ freeVars t1
				, freeVars t2
				, freeVars $ Clo.toClosure clo2_cut ]

		-- If a read or write effect acts on a region variable that 
		-- isn't visible to the calling context then we can mask it.
		maskable eff
			| [tc, TVar k (UVar v)]	<- takeTApps eff
			, elem tc [tRead, tWrite]
			, isRegionKind k
			, not $ Set.member v vsVisible
			= True
			
			| otherwise
			= False

		-- Converting to a sum and back to discard bottom effects.
		-- This makes the panic messages nicer.
		!eff2'	= flattenTSum $ makeTSum kEffect $ Foldable.toList eff2

		-- Mask effects on non-visible region.
		-- TODO: Gah @ needing to make a sum and flatten it again.
		---      We want an EffectStore similarly to the ClosureStore.
		!eff2_crushed	= flattenTSum $ makeTSum kEffect $ map crushT eff2'
		!eff2_masked	= makeTSum kEffect
				$ filter (not . maskable)
				$ eff2_crushed
		
		!effAnn_crushed	= crushT $ effAnn'
		!effSubs	= subsumesTT eff2_masked effAnn_crushed
		
		result
		 | not $ isSubsumes effSubs 
		 = panic stage $ vcat
			[ ppr "Effect mismatch in lambda abstraction."
			, "During:\n"				%> envCaller env, blank
			, "Effect of body:\n" 			%> eff2',	blank
			, "with closure:\n"			%> clo2_cut,	blank
			, "visible vars:\n"			%> vsVisible,	blank
			, "crushed effect:\n"			%> eff2_crushed,blank
			, "masked effect:\n"			%> eff2_masked,	blank
			, "does not match effect annotation:\n"	%> effAnn_crushed, blank
			, "in expression:\n"			%> xx,		blank]
		
		 | not $ isSubsumes cloSubs
		 = panic stage $ vcat
			[ ppr "Closure mismatch in lambda abstraction."
			, "During:\n"		 %> envCaller env, blank
			, "Closure of abstraction:\n"	 	%> clo2_cut,	blank
			, "is not less than annotation:\n" 	%> show cloAnn,	blank
			, "in expression:\n"			%> xx,		blank]

		 | otherwise
		 = 	( XLam v1  t1' x2' effAnn' cloAnn'
			, makeTFun t1' t2  effAnn' cloAnn'
		        , Seq.singleton tPure
		        , clo2_cut)
	  in result


	-- TODO: Carry a sequence of effects, and a map of closures back up the tree.
	--	 When we hit a lambda we can then flatten the effects.
	--	 For closures, we can delete the bound variable from the map then pass
	--	 it back up. Only put trimmed closures in the map.

	-- Value application
	-- TODO: why is it ok to discard the third closure?
	-- TODO: BAD! don't keep summing the same effect and closures, this calls nub.
	--       better to return a sequence on effects and only flatten them when we have to.
	XApp x1 x2
	 | (x1', t1, eff1, clo1) <- checkExp' n x1 env
	 , (x2', t2, eff2, clo2) <- checkExp' n x2 env
	 , t1'			 <- crushT $ trimClosureT_constrainForm $ toConstrainFormT t1
	 , t2'			 <- crushT $ trimClosureT_constrainForm $ toConstrainFormT t2
	 -> case takeTFun t1' of
	     Just (t11, t12, eff3, _)
	      -> case subsumesTT t2' t11 of
		  Subsumes 
		   -> ( XApp x1' x2'
		      , t12
		      , eff1 Seq.>< eff2 Seq.>< Seq.singleton eff3
		      , Clo.union clo1 clo2)
		
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

	-- Do expression
	XDo ss
	 | (ss', tLast, eff, clo)	<- checkStmts n ss env
	 -> 	( XDo ss'
		, tLast
		, eff
		, clo)

	-- Match expression
	XMatch aa
	 | (aa', tResult, eff, clo)	<- checkAlts n aa env
	 ->	( XMatch aa'
	 	, tResult
		, eff
		, clo)

	-- Local region binding.
	-- TODO: check r not free in type.
	-- TODO: mask effects on r.
	XLocal v bs x
	 | (x', t, eff, clo)	<- checkExp' n x env
	 -> let	
		maskable e
		 | TApp t1 (TVar kR (UVar vr))	<- e
		 , t1 == tRead || t1 == tWrite
		 , isRegionKind kR
		 , vr == v
		 = True
		
		 | otherwise
		 = False
		
		eff_masked	
			= filter (not . maskable)
			$ flattenTSum 
			$ crushT
			$ makeTSum kEffect 
			$ Foldable.toList eff
	
	    in	( XLocal v bs x'
		, t
		, Seq.fromList eff_masked
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
	 , tExp'		<- trimClosureT_constrainForm $ crushT $ toConstrainFormT tExp
	 -> kAnnot `seq`
	    if isEquiv $ equivTT tAnnot' tExp'
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
	-> ([Stmt], Type, Seq Effect, ClosureStore)

-- TODO: need to recursively add types to environment.
checkStmts n ss env
 = let	(ss', t, eff, clo)	
		= checkStmts' n env ss [] Seq.empty Clo.empty

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
	, effAcc Seq.>< eff 
	, Clo.union clo cloAcc)
	
checkStmts' n env (SBind Nothing x : ss) ssAcc effAcc cloAcc 
 = let	(x', t, eff, clo)	= checkExp' n x env
   in 	t `seq`
	checkStmts' n env ss 
		(SBind Nothing x' : ssAcc)
		(effAcc Seq.>< eff)
		(Clo.union clo cloAcc)

checkStmts' n env (SBind (Just v) x : ss) ssAcc effAcc cloAcc
 = let	(x', t, eff, clo)	= checkExp' n x env
   in	t `seq`
	withType v t env $ \env' -> 
	 checkStmts' n env' ss 
		(SBind (Just v) x' : ssAcc)
		(effAcc Seq.>< eff)
		(Clo.union clo cloAcc)


-- Alternatives -----------------------------------------------------------------------------------
-- | Check a list of match alternatives.
checkAlts 
	:: Int -> [Alt] -> Env
	-> ([Alt], Type, Seq Effect, ClosureStore)

checkAlts n as env
	= checkAlts' n env as [] [] Seq.empty Clo.empty

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
		(effGuards Seq.>< effAcc)
		(Clo.union cloGuards cloAcc)


-- Guards -----------------------------------------------------------------------------------------
-- | Check some pattern guards.
--   The variables bound by the pattern are in scope in successive guards.
checkGuards
	:: Int -> [Guard] -> Exp -> Env
	-> ([Guard], Exp, Type, Seq Effect, ClosureStore)

checkGuards n [] xBody env
 = let	(xBody', t, eff, clo)	= checkExp' (n+1) xBody env
   in	( []
	, xBody'
	, t
	, eff
	, clo)
	
checkGuards n (GExp (WVar v) x : gsRest) xBody env
 = let	(x', tExp, effExp, cloExp)	
		= checkExp' (n+1) x env

	(gsRest', xBody', tRest, effRest, cloRest)
		= withType v tExp env
		$ checkGuards n gsRest xBody
	
	eff'	= effExp Seq.>< effRest
	clo'	= Clo.mask v $ Clo.union cloExp cloRest
		
   in	( GExp (WVar v) x' : gsRest'
	, xBody'
	, tRest
	, eff'
	, clo')

-- TODO: Check the the expression has the type of the literal.
checkGuards n (GExp (WLit sp lf) x : gsRest) xBody env
 = let	(x', tExp, effExp, cloExp)
		= checkExp' (n+1) x env

	(gsRest', xBody', tRest, effRest, cloRest)
		= checkGuards n gsRest xBody env

	effMatch = effectOfMatchAgainst tExp
		
	eff'	= effExp Seq.>< Seq.singleton effMatch Seq.>< effRest
	clo'	= Clo.union cloExp cloRest
	
   in	( GExp (WLit sp lf) x' : gsRest'
	, xBody'
	, tRest
	, eff'
	, clo')

-- TODO: check against type of the pattern.
checkGuards n (GExp (WCon sp vCon lvts) x : gsRest) xBody env
 = let	(x', tExp, effExp, cloExp)	
		= checkExp' (n+1) x env

	-- TODO: add vars bound by pattern to environment.
	(gsRest', xBody', tRest, effRest, cloRest)
		= checkGuards n gsRest xBody env

	effMatch = effectOfMatchAgainst tExp
	eff'	= effExp Seq.>< Seq.singleton effMatch Seq.>< effRest

	-- Delete closure terms due to vars bound by the pattern.
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
	
	_ -> tPure

