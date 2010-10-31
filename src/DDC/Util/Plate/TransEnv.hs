
module DDC.Util.Plate.TransEnv
	( TransEnv(..)
	, TransEnvDown
	, TransEnvUp)
where

-- | Class of things that can be transformed while keeping track of an environment
--   which grows in a top-down manner.
class Monad m => TransEnv t m env a where
	transEnv :: t m env -> env -> a -> m a
	
-- | A transformation we can apply to a particular node of the tree.
--   It is given an environment that can be extended in a top-down manner.
type TransEnvDown m env a	
	= env -> a -> m (a, env)

type TransEnvUp   m env a
	= env -> a -> m a 
	

-- Instances ------------------------------------------------------------------
instance (  Monad m
	 ,  TransEnv t m env a
	 ,  TransEnv t m env b) 
 	 => TransEnv t m env (a, b) 
 where
 transEnv table env (x, y)
  = do	x'	<- transEnv table env x
	y'	<- transEnv table env y
	return	(x', y')

	
instance (  Monad m
	 ,  TransEnv t m env a) 
	 => TransEnv t m env [a] 
 where
 transEnv table env xx	= mapM (transEnv table env) xx

