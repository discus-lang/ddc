
module Core.Dictionary.Env
	( Env(..)
	, envGlobs)
where
import Core.Glob

data Env
	= Env 
	{ envHeaderGlob	:: Glob
	, envModuleGlob	:: Glob }

-- | Get a list of all the avaliable globs in an environment.
envGlobs :: Env -> [Glob]
envGlobs env	= [envHeaderGlob env, envModuleGlob env]
