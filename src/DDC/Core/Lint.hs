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
import Data.List
import Data.Maybe
import Control.Monad
import Core.Util		(maybeSlurpTypeX)
import Data.Map			(Map)
import Data.Sequence		(Seq)
import qualified Data.Sequence	as Seq
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import qualified Data.Foldable	as Foldable
import qualified Debug.Trace

stage		= "DDC.Core.Lint"

-- Set these to trace the checking of that sort to the console.

-- Glob -------------------------------------------------------------------------------------------
checkGlobs :: Glob -> Glob -> ()
checkGlobs cgHeader cgCore 	
--	= ()
	= checkList (checkBind (envInit cgHeader cgCore))
	$ Map.elems
	$ globBind cgCore


-- Top --------------------------------------------------------------------------------------------
checkBind :: Env -> Top -> ()
checkBind env pp
 = case pp of
	PBind v x
	 -> let Just t	= maybeSlurpTypeX x
	    in	checkTypeI 0 t env 
	  	`seq` withType v t env (checkExp x)
		`seq` ()

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
