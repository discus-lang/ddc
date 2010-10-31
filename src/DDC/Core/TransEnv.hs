{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Core.TransEnv
	(TransEnvCore(..))
where
import DDC.Util.Plate.TransEnv
import DDC.Type.TransEnv
import DDC.Core.Exp
import DDC.Type
import DDC.Var
import Control.Monad
import Data.Maybe


-- | Table containing functions we can apply to the various nodes of the tree.
data TransEnvCore m env
	= TransEnvCore
	{ transEnvT	:: Maybe (TransEnvType m env)	
		
	, transEnvP	:: TransEnvUp   m env Top
	, transEnvA	:: TransEnvUp   m env Alt
	, transEnvG	:: TransEnvUp   m env Guard
	, transEnvW	:: TransEnvUp   m env Pat 
	, transEnvS	:: TransEnvUp   m env Stmt

	, transEnvX_down	:: TransEnvDown m env Exp
	, transEnvX_up	:: TransEnvUp   m env Exp
	
	, transEnvV	:: TransEnvUp   m env Var }




-- Instances --------------------------------------------------------------------------------------
instance Monad m => TransEnv TransEnvCore m env Type where
 transEnv table env tt	= transEnv (fromMaybe transEnvTypeId (transEnvT table)) env tt

instance Monad m => TransEnv TransEnvCore m env Kind where
 transEnv table env tt	= transEnv (fromMaybe transEnvTypeId (transEnvT table)) env tt

instance Monad m => TransEnv TransEnvCore m env Bind where
 transEnv table env tt	= transEnv (fromMaybe transEnvTypeId (transEnvT table)) env tt

instance Monad m => TransEnv TransEnvCore m env Bound where
 transEnv table env tt	= transEnv (fromMaybe transEnvTypeId (transEnvT table)) env tt


instance Monad m => TransEnv TransEnvCore m env Exp where
 transEnv table env xx
  = do	(xx', env')	<- transEnvX_down table env xx
	let down :: TransEnv TransEnvCore m env a => a -> m a
	    down =  transEnv table env' 

	transEnvX_up table env' 
	 =<< case xx' of
		XNil			-> return xx'
		XVar   v  t		-> liftM2 XVar      (return v) (down t)
		XLit{}			-> return xx'
		XLAM   b k x		-> liftM3 XLAM      (down b)   (down k)  (down x)
		XAPP   x t		-> liftM2 XAPP      (down x)   (down t)
		XLam   v t x e c	-> liftM5 XLam      (return v) (down t)  (down x) (down e) (down c)
		XApp   x1 x2		-> liftM2 XApp      (down x1)  (down x2)
		XDo    ss		-> liftM  XDo       (down ss)
		XMatch alts		-> liftM  XMatch    (down alts)
		XLocal v  vts x		-> liftM3 XLocal    (down v)   (down vts) (down x)
		XPrim  p  xs		-> liftM2 XPrim     (return p) (down xs)
		XPrimType t      	-> liftM  XPrimType (down t)
		XTau   t  x		-> liftM2 XTau	    (down t)   (down x)


instance Monad m => TransEnv TransEnvCore m env Alt where
 transEnv = undefined

instance Monad m => TransEnv TransEnvCore m env Guard where
 transEnv = undefined

instance Monad m => TransEnv TransEnvCore m env Pat where
 transEnv = undefined

instance Monad m => TransEnv TransEnvCore m env Stmt where
 transEnv = undefined

instance Monad m => TransEnv TransEnvCore m env Var where
 transEnv = undefined


