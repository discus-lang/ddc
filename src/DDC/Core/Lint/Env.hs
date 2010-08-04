{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Core.Lint.Env
	( Env(..)
	, envInit
	, withType
	, withKindBound
	, typeFromEnv)
where
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Core.Glob
import DDC.Type
import DDC.Var
import Data.Map			(Map)
import qualified Data.Map	as Map

stage	= "DDC.Core.Lint.Env"

-- | A table of type and kind bindings.
data Env

	= Env
	{ -- | Whether the thing we're checking is supposed to be closed.
	  envClosed		:: Bool

	  --  The header glob, for getting top-level types and kinds.
	, envHeaderGlob		:: Glob

	  --  The core glob, for getting top-level types and kinds.
	, envModuleGlob		:: Glob

	  --  Types of value variables that are in scope at the current point.
	, envTypes		:: Map Var Type

	  -- | Kinds of type variables that are in scope at the current point,
	  ---  with optional :> constraint.
	, envKindBounds		:: Map Var (Kind, Maybe Type)}

envInit	cgHeader cgModule
	= Env
	{ envClosed		= False
	, envHeaderGlob		= cgHeader
	, envModuleGlob		= cgModule
	, envTypes		= Map.empty
	, envKindBounds		= Map.empty }


-- | Run a lint computation with an extra type in the environment.
withType :: Var -> Type -> Env -> (Env -> a) -> a
withType v t env fun
 = let	addVT Nothing	= Just t
	addVT Just{}	= panic stage $ "withVarType: type for " % v % " already present"
   in	fun $ env { envTypes = Map.alter addVT v (envTypes env) }


-- | Run a lint computation with an extra kind in the environment.
--   NOTE: We allow a type var to be rebound with the same kind, which makes
--         desugaring of projection puns easier.
--   TODO: we should probably redo the desugaring so this isn't needed.
--   TODO: we could also check we're not rebinding a type with a different :> constraint.
-- 
withKindBound :: Var -> Kind -> Maybe Type -> Env -> (Env -> a) -> a
withKindBound v k mt env fun
 = let	
	addVK Nothing	= Just (k, mt)
	addVK (Just (k', _))
	 | k == k'	= Just (k, mt)
	 | otherwise	= panic stage $ "withVarKind: " % v % " rebound with a different kind"

   in	fun $ env { envKindBounds = Map.alter addVK v (envKindBounds env) }


-- | Lookup the type of some value variable from the environment.
typeFromEnv :: Var -> Env -> Maybe Type
typeFromEnv v env
	| varNameSpace v /= NameValue
	= panic stage $ "lookupType: wrong namespace for " % v
	
	| Just t <- Map.lookup v $ envTypes env		= Just t
	| Just t <- typeFromGlob v $ envModuleGlob env	= Just t
	| Just t <- typeFromGlob v $ envHeaderGlob env	= Just t
	| otherwise					= Nothing
	
	
	

