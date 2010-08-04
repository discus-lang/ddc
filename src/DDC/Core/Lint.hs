-- {-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# OPTIONS -fno-warn-unused-binds -fno-warn-unused-imports #-}

-- | Check for type errors or other problems in a core program, and `panic`
--   if we find and. Also do a deepseq along the way. This module should
--   perform any possible internal consitency check we can think of on the 
--   core program.
--
--   TODO: Do full type checking.
--	   Check syntactic soundness of witnesses.
--	   Check for type vars that are out of scope
--
module DDC.Core.Lint
	( checkGlobs
	, checkExp, checkExp'
	, checkTypeI
	, checkKindI
	, Env	(..))
where
import Core.Util.Substitute
import Shared.VarPrim
import DDC.Core.Lint.Prim
import DDC.Core.Lint.Env
import DDC.Core.Lint.Base
import DDC.Core.Lint.Exp
import DDC.Core.Lint.Type
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Base.Literal
import DDC.Base.DataFormat
import DDC.Core.Glob
import DDC.Core.Exp
import DDC.Type
import DDC.Var
import Type.Error
import Data.List
import Data.Maybe
import Control.Monad
import DDC.Type.ClosureStore		(ClosureStore)
import Core.Util			(maybeSlurpTypeX)
import Data.Map				(Map)
import Data.Sequence			(Seq)
import qualified DDC.Type.ClosureStore	as Clo
import qualified Data.Sequence		as Seq
import qualified Data.Map		as Map
import qualified Data.Set		as Set
import qualified Data.Foldable		as Foldable
import qualified Debug.Trace

stage		= "DDC.Core.Lint"

-- Glob -------------------------------------------------------------------------------------------
checkGlobs :: Glob -> Glob -> Glob
checkGlobs cgHeader cgCore 	
	= mapBindsOfGlob (checkBind (envInit cgHeader cgCore)) cgCore
	

-- Top --------------------------------------------------------------------------------------------
checkBind :: Env -> Top -> Top
checkBind env pp
 = case pp of
	-- TODO: Check slurped type against reconstructed type.
	PBind v x
	 -> let Just tSlurped		= maybeSlurpTypeX x
	    	(tSlurped', k)		= checkTypeI 0 tSlurped env 
		(x', t', eff, clo)	= withType v t' env (checkExp x)
		
		-- We can mask effects on top level regions that are constant.
		maskable e
	 	 | TApp t1 (TVar kR (UVar vr))	<- e
		 , t1 == tRead
		 , isRegionKind kR
		 = isJust $ witnessConstFromEnv vr env
		
		 | otherwise
		 = False
		
		eff_masked	= makeTSum kEffect
				$ filter (not . maskable)
				$ flattenTSum 
				$ crushT eff
		
	    in	k `seq` t' `seq` eff `seq` clo `seq`
		if eff_masked == tPure 
			then PBind v x'
			else dieWithUserError [ErrorEffectfulCAF (v, t') eff_masked]

	_ -> panic stage $ "checkBind: no match"


{-
-- Var --------------------------------------------------------------------------------------------
-- | Lint a bound value variable.

lintBoundVar :: Var -> Env -> ()
lintBoundVar v env
 = case Map.lookup v (envTypes env) of
 	Nothing  -> panic stage $ "Variable " % v % " is not in scope.\n"
	Just _	 -> ()

-- | Lint a bound type variable.
lintBoundVarT :: Var -> Env -> ()
lintBoundVarT v env
 = case Map.lookup v (envKinds env) of
 	Nothing  -> panic stage $ "Variable " % v % " is not in scope.\n"
	Just _	 -> ()
-}
