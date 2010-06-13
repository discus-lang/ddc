
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
import DDC.Type
import DDC.Var
import Data.Maybe
import Data.Map			(Map)
import Data.Sequence		(Seq)
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
	 `seq` let !clo	= trimClosureC_constrainForm Set.empty Set.empty 
				$ makeTFree v 
				$ toConstrainFormT t
		   clo' 
			| isTBot clo				= Map.empty
			| Just (v', t')	<- takeTFree clo	= Map.singleton v' t'
			| otherwise = panic stage "checkExp[XVar]: no match"
			
	       in ( t, Seq.singleton tPure, clo')

	-- Type abstraction
	XLAM BNil k x
	 -> checkKindI n k env 
	 `seq` checkExp' n x env
	
	XLAM (BVar v) k x
	 -> checkKindI n k env
	 `seq` withKind v k env (checkExp' n x)

	-- Type application
	-- TODO: BAD! don't use substitute here. 
	--       better to propagate a list of constraints back up the tree.
	XAPP x t2
	 | (t1, eff, clo)	<- checkExp' n x env
	 , k2			<- checkTypeI n t2 env
	 -> case t1 of
		TForall BNil k11 _
		 | k11 == k2	-> (t1, eff, clo)
		
		TForall (BVar v) k11 t12
		 | k11 == k2	
		 -> ( substituteT (subSingleton v t2) t12
		    , fmap (substituteT (subSingleton v t2)) eff
		    , fmap (substituteT (subSingleton v t2)) clo)
		
		-- TODO: check more-than constraint
		TForall (BMore v _) k11 t12
		 | k11 == k2
		 -> ( substituteT (subSingleton v t2) t12
		    , fmap (substituteT (subSingleton v t2)) eff
		    , fmap (substituteT (subSingleton v t2)) clo)
		
		_ -> panic stage $ vcat
			[ ppr "Type error in application."
			, "Cannot apply:   " % t2
			, "to expression:  " % x
			, "in applicatoin: " % xx]
			

	-- Value abstraction
	-- TODO: check effect and closure annots
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

		   -- The effect returned from checkExp' will tend to contain a lot
		   -- of bottoms and repeated terms it. These get discarded by makeTSum.
		   !eff2'		= makeTSum kEffect $ Foldable.toList eff2
		   !effAnn'		= effAnn
		
		   -- The closure annotation on the abstraction is the closure of the body
		   -- minus the variable that is bound at this point.
		   !clo2_cut		= Map.delete v1 clo2
		   !cloAnn'		= slurpClosureToMap cloAnn

	       in  if effAnn' /= eff2'	
			then panic stage $ vcat
				[ ppr "Effect mismatch in lambda abstraction."
				, "Effect of body:\n" 			%> eff2',   blank
				, "does not match effect annotation:\n"	%> effAnn', blank
				, "in expression:\n"			%> xx,      blank]

	           else if cloAnn' /= clo2_cut	
			then panic stage $ vcat
				[ ppr "Closure mismatch in lambda abstraction."
				, "Closure of abstraction:\n"	 	%> clo2_cut, blank
				, "does not match annotation:\n" 	%> cloAnn',  blank
				, "in expression:\n"			%> xx,	      blank]

		   else ( makeTFun t1 t2 effAnn cloAnn
		        , Seq.singleton eff2'
		        , clo2)

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
	 -> case takeTFun t1 of
		Just (t11, t12, eff3, _)
		 | t11 == t2					-- TODO: use equiv
		 -> ( t12
		    , eff1 Seq.>< eff2 Seq.>< Seq.singleton eff3
		    , Map.union clo1 clo2)
		
		_ -> panic stage $ vcat
			[ ppr "Type error in application."
			, "           type: " % t1
			, " does not match: " % t2
			, " in application: " % xx]

	-- Do expression
	XDo ss		-> checkStmts n ss env

	-- Match expression
	XMatch aa	-> checkAlts  n aa env

	-- Local region binding.
	-- TODO: check r not free in type.
	-- TODO: mask effects on r.
	XLocal r ws x
	 -> let !(t1, eff, clo)	= checkExp' n x env
	    in	(t1, eff, clo)

	-- Primitive operator.
	XPrim  prim xs	-> checkPrim n prim xs env

	-- Type annotation
	XTau t x
	 ->    checkTypeI n t env
	 `seq` let !result@(t', _, _)	= checkExp' n x env
	       in if t == t'		then result		-- TODO: use equiv
		  else panic stage $ vcat
			[ ppr "Type error in type annotation.", blank
			, "  Reconstructed type:\n"		%> t', blank
			, "  does not match annotation:\n"	%> t,  blank
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
	= checkStmts' n env ss Seq.empty Map.empty

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
-- TODO: handle guards.
-- TODO: add effect from the match.
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
 = let	
	-- Check the guards. 
	-- The variables bound by the patterns are in scope in successive guards,
	-- as well as in the right (body) of the alternative.
	(env', effsGuard, closGuard)
		= checkGuards n gs env
	
	(tBody, effBody, cloBody)
		= checkExp' (n+1) x env'

   in	checkAlts' n env as 
		(tBody : types)
		(effsGuard Seq.>< effAcc Seq.>< effBody)
		(Map.unions [cloBody, cloAcc, closGuard])


-- Guards -----------------------------------------------------------------------------------------
-- | Check some pattern guards.
--   The variables bound by the pattern are in scope in successive guards.
checkGuards :: Int -> [Guard] -> Env -> (Env, Seq Effect, Map Var Closure)
checkGuards n gs env
	= checkGuards' n gs env Seq.empty Map.empty
	
checkGuards' _ [] env effAcc cloAcc
	= (env, effAcc, cloAcc)

checkGuards' n (GExp pat x : rest) env effAcc cloAcc
 = let	(_, effExp, cloExp)	= checkExp' (n + 1) x env
	
	-- TODO: check against type of pattern,
	--	 and add the vars to the environment.
	
   in	checkGuards' n rest env 
		(effExp Seq.>< effAcc)
		(Map.union cloExp cloAcc)

