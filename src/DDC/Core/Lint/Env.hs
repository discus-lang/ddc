{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Core.Lint.Env
	( Env(..)
	, envInit
	, withType
	, withBound
	, withKind)
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

	  -- | Kinds of type variables that are in scope at the current point.
	, envKinds		:: Map Var Kind }

envInit	cgHeader cgModule
	= Env
	{ envClosed		= False
	, envHeaderGlob		= cgHeader
	, envModuleGlob		= cgModule
	, envTypes		= Map.empty
	, envKinds		= Map.empty }


-- | Run a lint computation with an extra type in the environment.
withType :: Var -> Type -> Env -> (Env -> a) -> a
withType v t env fun
 = let	addVT Nothing	= Just t
	addVT Just{}	= panic stage $ "withVarType: type for " % v % " already present"
   in	fun $ env { envTypes = Map.alter addVT v (envTypes env) }


withBound :: Var -> Type -> Env -> (Env -> a) -> a
withBound = error "no withBound"

-- | Run a lint computation with an extra kind in the environment.
withKind :: Var -> Kind -> Env -> (Env -> a) -> a
withKind v k env fun
 = let	addVK Nothing	= Just k
	addVK Just{}	= panic stage $ "withVarKind: kind for " % v % " already present"
   in	fun $ env { envKinds = Map.alter addVK v (envKinds env) }


