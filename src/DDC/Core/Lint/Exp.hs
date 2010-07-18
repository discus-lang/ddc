{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Core.Lint.Exp
	( checkExp
	, checkExp')	-- used by DDC.Core.Lint.Prim
where
import Core.Util.Substitute
import DDC.Main.Error
import DDC.Main.Pretty
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
import Data.Map			(Map)
import Data.Sequence		(Seq)
import qualified Shared.VarUtil	as Var
import qualified Data.Sequence	as Seq
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import qualified Data.Foldable	as Foldable

stage	= "DDC.Core.Lint.Exp"

-- Exp --------------------------------------------------------------------------------------------
-- | Check an expression, returning its type.
checkExp :: Exp -> Env -> (Type, Effect, Closure)
checkExp xx env
 = let	-- NOTE: This binding must be strict because of the way checkBind discards
	--	 the result of checkExp above.
	!(t, effs, mclo)	= checkExp' 0 xx env
   in	( t
	, makeTSum kEffect $ Foldable.toList effs
	, makeTSum kClosure  [makeTFree v t' | (v, t') <- Map.toList mclo])

checkExp' 
	:: Int 			-- indent level for tracing
	-> Exp			-- expression to check
	-> Env			-- environment
	-> ( Type		-- type of expression
	   , Seq Effect		-- effect of expression
	   , Map Var Type)	-- closure of expression,

checkExp' n xx env
 = if debugExp
    then let !result@(t, eff, clo)	
		= trace (setColumn (n*indenting) % vcat
			[ ppr (replicate 70 '-') <> "Exp" <> n
			, ppr xx ])
		$ checkExp_trace n xx env

	 in trace 
		(setColumn (n*indenting) % vcat 
		[ "type:   " 	% t
		, "effect: "	% eff
		, "closure:\n"	%> vcat (map ppr $ Map.toList clo)
		, ppr xx
		, "--" <> "Exp" <> n <> ppr (replicate 70 '-')])
		result
   else	checkExp_trace n xx env



checkExp_trace :: Int -> Exp -> Env -> (Type, Seq Effect, Map Var Type)		
checkExp_trace m xx env
 = let n	= m + 1
   in case xx of
	XNil	-> panic stage "checkExp: XNil"
	
	-- Variables
	XVar v t
	 | varNameSpace v /= NameValue
	 -> panic stage 
		$ "checkExp: invalid namespace for variable " 
		% v <> (show $ varNameSpace v)
	
	 -- TODO: the type should have value kind.
	 -- TODO: make a version of the trimmer that doesn't need the initial sets.
	 | otherwise
	 -> checkTypeI n t env 
	 `seq` let !clo	= trimClosureC_constrainForm
				$ makeTFree v 
				$ toConstrainFormT t
			
		   isFreeDanger c
			| Just (_, t')	<- takeTFree c
			, isJust $ takeTDanger t'
			= True
			
			| otherwise
			= False

		   clo_dump
		 	= packType
			$ makeTSum kClosure 
			$ filter (not . isFreeDanger) 
			$ flattenTSum clo		  

		   -- TODO: we're ignoring closure terms due to constructors
		   --       of arity zero, like True :: Bool %r1.
		   --       Is is good enough to say this is ok because the region is always constant?
		   clo' | isTBot clo_dump	= Map.empty
			| Var.isCtorName v	= Map.empty
			
			-- If the trimmer closure just has a single part then add that.
			| Just (v', t')		<- takeTFree clo_dump
			= Map.singleton v' t'

			-- Otherwise the closure will be a sum. 
			| otherwise		= Map.singleton v  clo_dump
			
	       in ( t, Seq.singleton tPure, clo')


	-- Literal values
	-- HACK: Handle applications of string literals to their regions directly
	--       so we don't have to come up with its actual type scheme. Doing this
	--       would require a fresh region variable...
	XAPP (XLit (LiteralFmt LString{} Unboxed)) (TVar k r)
	 | k == kRegion
	 , Just tc	<- tcString Unboxed
	 -> ( TApp (TCon tc) (TVar kRegion r)
	    , Seq.empty
	    , Map.empty)


	XLit litFmt
	 -> let	Just tcLit	= tyConOfLiteralFmt litFmt
	    in	( TCon tcLit
		, Seq.singleton tPure
		, Map.empty)

	-- Type abstraction
	XLAM BNil k x
	 -> checkKindI n k env 
	 `seq` checkExp' n x env
	
	XLAM (BVar v) k x
	  ->    checkKindI n k env
	  `seq` withKind v k env (checkExp' n x)

	-- Type application
	-- TODO: BAD! don't use substitute here. 
	--       better to propagate a list of constraints back up the tree.
	XAPP x t2
	 | (t1, eff, clo)	<- checkExp' n x env
	 , k2			<- crushK $ checkTypeI n t2 env
	 -> case t1 of
		TForall BNil k11 t12
		 | isEquiv $ equivKK (crushK k11) k2	
		 -> (t12, eff, clo)
		
		TForall (BVar v) k11 t12
		 | isEquiv $ equivKK (crushK k11) k2	
		 -> ( substituteT (subSingleton v t2) t12
		    , fmap (substituteT (subSingleton v t2)) eff
		    , fmap (substituteT (subSingleton v t2)) clo)
		
		-- TODO: check more-than constraint
		TForall (BMore v _) k11 t12
		 | isEquiv $ equivKK (crushK k11) k2
		 -> ( substituteT (subSingleton v t2) t12
		    , fmap (substituteT (subSingleton v t2)) eff
		    , fmap (substituteT (subSingleton v t2)) clo)
		
		_ -> panic stage $ vcat
			[ ppr "Type mismatch in (value/type) application."
			, "Cannot apply type:\n" %> t2,	blank
			, "of kind:\n"		 %> k2, blank
			, "to expression:\n" 	 %> x,	blank
			, "with type:\n"	 %> t1,	blank]
			

	-- Value abstraction
	XLam v1 t1 x2 effAnn cloAnn
	 | varNameSpace v1 /= NameValue
	 -> panic stage
		$ "checkExp invalid namespace for variable "
		% v1 <> (show $ varNameSpace v1)
		
	 -- TODO: check kinds of these types
	 --	  use function checkTypeHasKind
	 | otherwise
	 ->    checkTypeI n t1 env
	 `seq` checkTypeI n effAnn env
	 `seq` checkTypeI n cloAnn env 
	 `seq` let !(t2, eff2, clo2)	= withType v1 t1 env (checkExp' n x2)

		   -- The closure annotation on the abstraction is the closure of the body
		   -- minus the variable that is bound at this point.
		   !clo2_cut	= Map.delete v1 clo2
		   !cloAnn'	= slurpClosureToMap cloAnn

		   -- The visible region variables.
		   -- These are vars that are present in the parameter or return type, 
		   -- or in one of the types in the closure.
		   vsVisible	= Set.filter (\v -> varNameSpace v == NameRegion)
				$ Set.unions
					[ freeVars t1
					, freeVars t2
					, freeVars $ Map.elems clo2_cut ]

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
		   !eff2'	= flattenTSum
				$ makeTSum kEffect 
				$ Foldable.toList eff2

		   -- Mask effects on non-visible region.
		   !eff2_masked	= makeTSum kEffect
				$ filter (not . maskable)
				$ map crushT eff2'
		
		   !effAnn'	= crushT $ effAnn
		   !effEquiv	= equivTT effAnn' eff2_masked
		
	       in  if not $ isEquiv effEquiv	
			then panic stage $ vcat
				[ ppr "Effect mismatch in lambda abstraction."
				, "Effect of body:\n" 			%> eff2',       blank
				, "with closure:\n"			%> clo2_cut,    blank
				, "visible vars:\n"			%> vsVisible,   blank
				, "masked effect:\n"			%> eff2_masked, blank
				, "does not match effect annotation:\n"	%> effAnn',     blank
				, "in expression:\n"			%> xx,          blank]

	           else if cloAnn' /= clo2_cut	
			then panic stage $ vcat
				[ ppr "Closure mismatch in lambda abstraction."
				, "Closure of abstraction:\n"	 	%> clo2_cut,  blank
				, "does not match annotation:\n" 	%> cloAnn',   blank
				, "in expression:\n"			%> xx,	      blank]

		   else ( makeTFun t1 t2 effAnn cloAnn
		        , Seq.singleton tPure
		        , clo2_cut)

	-- TODO: Carry a sequence of effects, and a map of closures back up the tree.
	--	 When we hit a lambda we can then flatten the effects.
	--	 For closures, we can delete the bound variable from the map then pass
	--	 it back up. Only put trimmed closures in the map.

	-- Value application
	-- TODO: why is it ok to discard the third closure?
	-- TODO: BAD! don't keep summing the same effect and closures, this calls nub.
	--       better to return a sequence on effects and only flatten them when we have to.
	XApp x1 x2
	 | (t1, eff1, clo1)	<- checkExp' n x1 env
	 , (t2, eff2, clo2)	<- checkExp' n x2 env
	 , t2'			<- crushT $ trimClosureT_constrainForm t2
	 -> case takeTFun t1 of
	     Just (t11, t12, eff3, _)
	      -> case subsumesTT t2' t11 of
		  Subsumes 
		   -> ( t12
		      , eff1 Seq.>< eff2 Seq.>< Seq.singleton eff3
		      , Map.union clo1 clo2)
		
		  NoSubsumes s1 s2
		   -> panic stage $ vcat
			[ ppr "Type error in application."
			, "  cannot apply function of type: " % t1
			, "            to argument of type: " % t2'
			, ppr "   because"
			, "                           type: " % s2
			, "               does not subsume: " % s1 
			, blank
			, "in application:\n" % xx]
		
	     _ -> panic stage $ vcat
			[ ppr "Type error in application."
			, "  cannot apply non-function type: " % t1
			, "             to argument of type: " % t2'
			, blank
			, " in application: " % xx]

	-- Do expression
	XDo ss		-> checkStmts n ss env

	-- Match expression
	XMatch aa	-> checkAlts  n aa env

	-- Local region binding.
	-- TODO: check r not free in type.
	-- TODO: mask effects on r.
	XLocal _ _ x
	 -> let !(t1, eff, clo)	= checkExp' n x env
	    in	(t1, eff, clo)

	-- Primitive operator.
	XPrim  prim xs	-> checkPrim n prim xs env

	-- Type annotation
	XTau tAnnot x
	 ->    checkTypeI n tAnnot env
	 `seq` let !result@(tExp, _, _)	= checkExp' n x env
		   tExp'		= trimClosureT_constrainForm 
					$ toConstrainFormT tExp

	       in if isEquiv $ equivTT tAnnot tExp' then result
		  else panic stage $ vcat
			[ ppr "Type error in type annotation.", blank
			, "  Reconstructed type:\n"		%> tExp',   blank
			, "  does not match annotation:\n"	%> tAnnot,  blank
			, "  on expression:\n"			%> xx]
		
	_ -> panic stage 
		$ vcat 	[ ppr "checkExp: no match for:"
		  	, ppr xx]
			-- , ppr $ show xx]
		


-- Statements -------------------------------------------------------------------------------------
-- | Check a list of (possibly recursive) statements.
checkStmts :: Int -> [Stmt] -> Env -> (Type, Seq Effect, Map Var Closure)

-- TODO: need to recursively add types to environment.
checkStmts n ss env
 = let	(t, eff, clo)	= checkStmts' n env ss Seq.empty Map.empty

	-- Delete closure terms due to variables bound by the statements.
	vsBound		= [v | SBind (Just v) _ <- ss ]
  	clo'		= foldr Map.delete clo vsBound

   in	(t, eff, clo')

checkStmts' _ _ [] _ _
 	= panic stage
	$ "checkStmts': no statements"
	
checkStmts' n env (SBind _ x : []) effAcc cloAcc
 = let	(t, eff, clo)	= checkExp' (n+1) x env
   in	( t
	, effAcc Seq.>< eff 
	, Map.union clo cloAcc)
	
-- types for all bindings must already be in environment.
checkStmts' n env (SBind Nothing x : ss) effAcc cloAcc 
 = let	(_, eff, clo)	= checkExp' n x env
   in 	checkStmts' n env ss (effAcc Seq.>< eff) (Map.union clo cloAcc)

-- TODO: check type against on already in environment.
checkStmts' n env (SBind (Just _) x : ss) effAcc cloAcc
 = let	(_, eff, clo)	= checkExp' n x env
   in	checkStmts' n env ss (effAcc Seq.>< eff) (Map.union clo cloAcc)


-- Alternatives -----------------------------------------------------------------------------------
-- | Check a list of match alternatives.
checkAlts :: Int -> [Alt] -> Env -> (Type, Seq Effect, Map Var Closure)
checkAlts n as env
	= checkAlts' n env as [] Seq.empty Map.empty

checkAlts' _ _ [] types effAcc cloAcc
 = 	( fromMaybe 
		(panic stage $ "checkAlts: can't join types")
		(joinSumTs types)
	, effAcc
	, cloAcc)

checkAlts' n env (AAlt gs x : as) types effAcc cloAcc
 = let	(tBody, effGuards, cloGuards)
		= checkGuards n gs x env
	
   in	checkAlts' n env as 
		(tBody : types)
		(effGuards Seq.>< effAcc)
		(Map.union cloGuards cloAcc)


-- Guards -----------------------------------------------------------------------------------------
-- | Check some pattern guards.
--   The variables bound by the pattern are in scope in successive guards.
checkGuards :: Int -> [Guard] -> Exp -> Env -> (Type, Seq Effect, Map Var Closure)
checkGuards n [] xBody env
	= checkExp' (n+1) xBody env
	
checkGuards n (GExp (WVar v) x : rest) xBody env
 = let	(tExp, effExp, cloExp)	
		= checkExp' (n+1) x env

	(tRest, effRest, cloRest)
		= withType v tExp env
		$ checkGuards n rest xBody
	
	eff'	= effExp Seq.>< effRest
	clo'	= Map.delete v $ Map.union cloExp cloRest
		
   in	(tRest, eff', clo')

-- TODO: Check the the expression has the type of the literal.
checkGuards n (GExp (WLit _ _) x : rest) xBody env
 = let	(tExp, effExp, cloExp)
		= checkExp' (n+1) x env

	(tRest, effRest, cloRest)
		= checkGuards n rest xBody env

	effMatch = effectOfMatchAgainst tExp
		
	eff'	= effExp Seq.>< Seq.singleton effMatch Seq.>< effRest
	clo'	= Map.union cloExp cloRest
	
   in	(tRest, eff', clo')

-- TODO: check against type of the pattern.
checkGuards n (GExp (WCon _ _vCon lvts) x : rest) xBody env
 = let	(tExp, effExp, cloExp)	
		= checkExp' (n+1) x env

	-- TODO: add vars bound by pattern to environment.
	(tRest, effRest, cloRest)
		= checkGuards n rest xBody env

	effMatch = effectOfMatchAgainst tExp
	eff'	= effExp Seq.>< Seq.singleton effMatch Seq.>< effRest

	-- Delete closure terms due to vars bound by the pattern.
	vsBound	= [v | (_, v, _) <- lvts]
	clo	= Map.union cloExp cloRest
	clo'	= foldr Map.delete clo vsBound

   in	(tRest, eff', clo')


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

