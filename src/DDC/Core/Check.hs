{-# OPTIONS -fno-warn-unused-binds -fno-warn-unused-imports -fwarn-name-shadowing #-}

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
import Core.Util.Substitute
import Shared.VarPrim
import DDC.Core.Check.Prim
import DDC.Core.Check.Env
import DDC.Core.Check.Base
import DDC.Core.Check.Exp
import DDC.Core.Check.Type
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

