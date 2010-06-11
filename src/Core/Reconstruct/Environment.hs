
-- | Environment for type reconstruction
module Core.Reconstruct.Environment
	( Env
	, initEnv
	, getEnvCaller
	, addWitnessConst, 	lookupWitnessConst
	, addEqVT,		lookupEqVT
	, addMoreVT, addMoreF,	lookupMoreVT)
where
import DDC.Core.Glob
import DDC.Type
import DDC.Var
import qualified Data.Map	as Map
import Data.Map			(Map)
import Util.Data.Maybe

data Env
	= Env
	{ -- | The name of the function that initiated the reconstruction, for debugging.
	  envCaller	:: Maybe String

	  -- | The header glob, containing types of imported stuff.
	, envGlobHeader	:: Glob 

	  -- | The glob of the thing we're reconstructing.
	, envGlobCore	:: Glob
	
	  -- | Map of regions to the witness variables that prove their constancy.
	, envLocalWitnessConst :: Map Var Var 
	
	  -- | Map of type equalities defined locally.
	, envLocalEq	:: Map Var Type

	  -- | Map of more-than constraints defined locally
	, envLocalMore	:: Map Var Type }
	
	
-- | Create the initial environment for type reconstruction.
initEnv :: Maybe String	-- ^ Name of the function that initiated the reconstruction.
	-> Glob		-- ^ Header glob.
	-> Glob		-- ^ Core glob.
	-> Env		-- ^ Initial environment.

initEnv caller cgHeader cgCore 
 	= Env
	{ envCaller		= caller
	, envGlobHeader		= cgHeader
	, envGlobCore		= cgCore
	, envLocalWitnessConst	= Map.empty
	, envLocalEq		= Map.empty
	, envLocalMore		= Map.empty }
	

-- | Get the caller name from the environment.
getEnvCaller :: Env -> Maybe String
getEnvCaller env = envCaller env


-- Constancy --------------------------------------------------------------------------------------
-- | Add a witness of constancy to the environment.
addWitnessConst 
	:: Var		-- ^ Region variable. 
	-> Var 		-- ^ Variable that binds the witness of constancy for that region.
	-> Env -> Env
addWitnessConst vRegion vWitness env
 = env { envLocalWitnessConst 
		= Map.insert vRegion vWitness 
			(envLocalWitnessConst env) }
	

-- | Lookup a witness of constancy for a given region variable.
lookupWitnessConst :: Var -> Env -> Maybe Var
lookupWitnessConst v env
	= Map.lookup v (envLocalWitnessConst env)
	
	
-- Equality ---------------------------------------------------------------------------------------
-- | Add a local type equality to the environment.
addEqVT :: Var -> Type -> Env -> Env
addEqVT v t env
 = env	{ envLocalEq
		= Map.insert v t (envLocalEq env) }


-- | Lookup a type equality constraint from the environment.
lookupEqVT :: Var -> Env -> Maybe Type
lookupEqVT v env
 	= takeFirstJust 
	[ Map.lookup   v (envLocalEq    env)
	, typeFromGlob v (envGlobCore   env)
	, typeFromGlob v (envGlobHeader env) ]


-- More-than --------------------------------------------------------------------------------------
-- | Add a local more-than constraint to the environment.
addMoreVT :: Var -> Type -> Env -> Env
addMoreVT v t env
 = env	{ envLocalMore 
		= Map.insert v t (envLocalMore env) }


-- | Add a type inequality from a fetter to the environment.
addMoreF :: Fetter -> Env -> Env
addMoreF ff env
 = case ff of
	FMore (TVar _ (UVar v)) t	-> addMoreVT v t env
	_				-> env
	
	
-- | Lookup a type more-than constraint from the environment.	
lookupMoreVT :: Var -> Env -> Maybe Type
lookupMoreVT v env
	= Map.lookup v (envLocalMore env)


	
	