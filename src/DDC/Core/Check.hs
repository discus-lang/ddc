-- | Check for type errors or other problems in a core program, and `panic`
--   if we find any.
--
--   TODO: Check syntactic soundness of witnesses.
--         Optionally check for fabricated witnesses.
--	   Check for type vars that are out of scope.
--
module DDC.Core.Check
	( checkGlobs
	, checkExp, checkExp'
	, checkOpenExp
	, checkTypeI
	, checkKindI
	, checkedTypeOfExp 
	, checkedTypeOfOpenExp
	, Env	(..))
where
import DDC.Core.Check.Env
import DDC.Core.Check.Exp
import DDC.Core.Check.Type
import DDC.Main.Error
import DDC.Core.Glob
import DDC.Core.Exp
import DDC.Type
import DDC.Solve.Error
import Data.Maybe

stage		= "DDC.Core.Lint"

-- Glob -------------------------------------------------------------------------------------------
-- | Given the name of the caller for panic messages, check some header and core globs.
checkGlobs :: String -> Glob -> Glob -> Glob
checkGlobs caller cgHeader cgCore 	
 = let	env	= envInit caller cgHeader cgCore
   in	mapBindsOfGlob (checkBind env) cgCore
	

-- Top --------------------------------------------------------------------------------------------
-- | Check a top level binding.
checkBind :: Env -> Top -> Top
checkBind env pp
 = case pp of
	PBind v x
	 -> let	(x', t', eff, clo)	= withType v t' env (checkExp x)
		
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
		
	    in	t' `seq` eff `seq` clo `seq`
		if eff_masked == tPure 
			then PBind v x'
			else dieWithUserError [ErrorEffectfulCAF (v, t') eff_masked]

	_ -> panic stage $ "checkBind: no match"

