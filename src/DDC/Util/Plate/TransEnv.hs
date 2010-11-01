
module DDC.Util.Plate.TransEnv
	( TransEnv(..)
	, TransEnvDown
	, TransEnvUp)
where
import Control.Monad


-- | Class of things that can be transformed while keeping track of an environment
--   which grows in a top-down manner.
class Monad m => TransEnv t m env a where
	transEnv :: t m env -> env -> a -> m a
	
-- | Transform that is applied to a node of the tree on the way down.
--   It is given an environment that can be extended in a top-down manner.
type TransEnvDown m env a	
	= env -> a -> m (a, env)

-- | Tranform that is applied to a node of the tree on the way up.
type TransEnvUp   m env a
	= env -> a -> m a 
	

-- Instances ------------------------------------------------------------------

-- (,)
instance (  Monad m
	 ,  TransEnv t m env a
	 ,  TransEnv t m env b) 
 	 => TransEnv t m env (a, b) 
 where
 transEnv table env (x, y)
  = do	x'	<- transEnv table env x
	y'	<- transEnv table env y
	return	(x', y')

-- (,,)
instance (  Monad m
	 ,  TransEnv t m env a
	 ,  TransEnv t m env b
	 ,  TransEnv t m env c)
 	 => TransEnv t m env (a, b, c) 
 where
 transEnv table env (x, y, z)
  = do	x'	<- transEnv table env x
	y'	<- transEnv table env y
	z'	<- transEnv table env z
	return	(x', y', z')


-- []	
instance (  Monad m
	 ,  TransEnv t m env a) 
	 => TransEnv t m env [a] 
 where
 transEnv table env xx	= mapM (transEnv table env) xx


-- Maybe
instance (  Monad m
	 ,  TransEnv t m env a) 
	 => TransEnv t m env (Maybe a)
 where
 transEnv table env xx
  = case xx of
	Nothing	-> return Nothing
	Just x	-> liftM Just (transEnv table env x)
	

