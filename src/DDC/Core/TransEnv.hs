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
	{ transEnvCoreT		:: Maybe (TransEnvType m env)	
		
	, transEnvCoreP		:: TransEnvUp   m env Top
	, transEnvCoreA		:: TransEnvUp   m env Alt
	, transEnvCoreG		:: TransEnvUp   m env Guard
	, transEnvCoreW		:: TransEnvUp   m env Pat 
	, transEnvCoreS		:: TransEnvUp   m env Stmt

	, transEnvCoreX_down	:: TransEnvDown m env Exp
	, transEnvCoreX_up	:: TransEnvUp   m env Exp
	
	, transEnvCoreV		:: TransEnvUp   m env Var }


-- Instances --------------------------------------------------------------------------------------

-- Pass of type stuff to the transEnvT table.
instance Monad m => TransEnv TransEnvCore m env Type where
 transEnv table env tt	= transEnv (fromMaybe transEnvTypeId (transEnvCoreT table)) env tt

instance Monad m => TransEnv TransEnvCore m env Kind where
 transEnv table env tt	= transEnv (fromMaybe transEnvTypeId (transEnvCoreT table)) env tt

instance Monad m => TransEnv TransEnvCore m env Bind where
 transEnv table env tt	= transEnv (fromMaybe transEnvTypeId (transEnvCoreT table)) env tt

instance Monad m => TransEnv TransEnvCore m env Bound where
 transEnv table env tt	= transEnv (fromMaybe transEnvTypeId (transEnvCoreT table)) env tt


-- Exp
instance Monad m => TransEnv TransEnvCore m env Exp where
 transEnv table env xx
  = do	(xx', env')	<- transEnvCoreX_down table env xx
	let down :: TransEnv TransEnvCore m env a => a -> m a
	    down =  transEnv table env' 

	transEnvCoreX_up table env' 
	 =<< case xx' of
		XNil			-> return xx'
		XVar   v  t		-> liftM2 XVar      (down v)   (down t)
		XLit{}			-> return xx'
		XLAM   b k x		-> liftM3 XLAM      (down b)   (down k)  (down x)
		XAPP   x t		-> liftM2 XAPP      (down x)   (down t)
		XLam   v t x e c	-> liftM5 XLam      (down v)   (down t)  (down x) (down e) (down c)
		XApp   x1 x2		-> liftM2 XApp      (down x1)  (down x2)
		XDo    ss		-> liftM  XDo       (down ss)
		XMatch alts		-> liftM  XMatch    (down alts)
		XLocal v  vts x		-> liftM3 XLocal    (down v)   (down vts) (down x)
		XPrim  p  xs		-> liftM2 XPrim     (return p) (down xs)
		XPrimType t      	-> liftM  XPrimType (down t)
		XTau   t  x		-> liftM2 XTau	    (down t)   (down x)

-- Stmt
instance Monad m => TransEnv TransEnvCore m env Stmt where
 transEnv table env ss
  = do 	let down :: TransEnv TransEnvCore m env a => a -> m a
	    down =  transEnv table env

	case ss of
		SBind mv x	-> liftM2 SBind (down mv) (down x)


-- Alt
instance Monad m => TransEnv TransEnvCore m env Alt where
 transEnv table env aa
  = do	let down :: TransEnv TransEnvCore m env a => a -> m a
	    down =  transEnv table env

	case aa of
		AAlt gs x	-> liftM2 AAlt (down gs) (down x)
		
-- Guard
instance Monad m => TransEnv TransEnvCore m env Guard where
 transEnv table env gg
  = do	let down :: TransEnv TransEnvCore m env a => a -> m a
	    down =  transEnv table env

	case gg of
		GExp ws x	-> liftM2 GExp (down ws) (down x)

-- Pat
instance Monad m => TransEnv TransEnvCore m env Pat where
 transEnv table env ww
  = do	let down :: TransEnv TransEnvCore m env a => a -> m a
	    down =  transEnv table env

	case ww of
		WVar v		-> liftM WVar (down v)
		WLit{}		-> return ww
		WCon sp v lvts	-> liftM3 WCon (return sp) (down v) (down lvts)

-- Label
instance Monad m => TransEnv TransEnvCore m env Label where
 transEnv table env ll
  = do	let down :: TransEnv TransEnvCore m env a => a -> m a
	    down =  transEnv table env

	case ll of
		LIndex{}	-> return ll
		LVar v		-> liftM LVar (down v)
		
-- Var
instance Monad m => TransEnv TransEnvCore m env Var where
 transEnv table env vv	= transEnvCoreV table env vv

