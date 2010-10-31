{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DDC.Type.TransEnv
	( TransEnvType(..)
	, transEnvTypeId)
where
import DDC.Util.Plate.TransEnv
import DDC.Type
import DDC.Var
import Control.Monad


-- | Table containing the functions we need to transEnvform type expressions.
data TransEnvType m env
	= TransEnvType
	{ transEnvTypeV		:: TransEnvUp   m env Var

	, transEnvTypeT_down	:: TransEnvDown m env Type
	, transEnvTypeT_up	:: TransEnvUp   m env Type

	, transEnvTypeK_down	:: TransEnvDown m env Kind
	, transEnvTypeK_up	:: TransEnvUp   m env Kind }
	

-- | Identity transEnvformation table.
transEnvTypeId :: Monad m => TransEnvType m env
transEnvTypeId
	= TransEnvType
	{ transEnvTypeV		= \_   x -> return x

	, transEnvTypeT_down	= \env x -> return (x, env)
	, transEnvTypeT_up	= \_   x -> return x

	, transEnvTypeK_down	= \env x -> return (x, env)
	, transEnvTypeK_up	= \_   x -> return x }


-- Instances ------------------------------------------------------------------
instance Monad m => TransEnv TransEnvType m env Super where
 transEnv table env ss
  = do	let down :: TransEnv TransEnvType m env a => a -> m a
	    down =  transEnv table env

	case ss of
		SProp		-> return ss
		SBox		-> return ss
		SFun k s	-> liftM2 SFun (down k) (down s)


instance Monad m => TransEnv TransEnvType m env Kind where
 transEnv table env kk
  = do	(kk', env')	<- transEnvTypeK_down table env kk
	let down :: TransEnv TransEnvType m env a => a -> m a
	    down =  transEnv table env'

	transEnvTypeK_up table env'
	 =<< case kk' of
		KNil		-> return kk'
		KCon kc s	-> liftM2 KCon (return kc) (down s)
		KFun k1 k2	-> liftM2 KFun (down k1)   (down k2)
		KApp k1 t2	-> liftM2 KApp (down k1)   (down t2)
		KSum ks		-> liftM  KSum (down ks)


instance Monad m => TransEnv TransEnvType m env Type where
 transEnv table env tt
  = do	(tt', env')	<- transEnvTypeT_down table env tt
	let down :: TransEnv TransEnvType m env a => a -> m a
	    down =  transEnv table env'

	transEnvTypeT_up table env' 
	 =<< case tt' of
		TNil              -> return tt'
	 	TVar k b          -> liftM2 TVar       (down k)  (down b)
	 	TCon tc           -> liftM  TCon       (down tc)
	 	TSum k ts         -> liftM2 TSum       (down k)  (down ts)
	 	TApp t1 t2        -> liftM2 TApp       (down t1) (down t2)
	 	TForall b k t     -> liftM3 TForall    (down b)  (down k)  (down t)
	 	TConstrain t crs  -> liftM2 TConstrain (down t)  (down crs)
	 	TError k err      -> liftM2 TError     (down k)  (return err)
	
	
instance Monad m => TransEnv TransEnvType m env Constraints where
 transEnv = undefined


instance Monad m => TransEnv TransEnvType m env Var where
 transEnv table env v 	= transEnvTypeV table env v


instance Monad m => TransEnv TransEnvType m env Bind where
 transEnv table env bb
  = do	let down :: TransEnv TransEnvType m env a => a -> m a
	    down =  transEnv table env
	
	case bb of
		BNil		-> return bb
		BVar  v		-> liftM  BVar  (down v)
		BMore v t	-> liftM2 BMore (down v) (down t)


instance Monad m => TransEnv TransEnvType m env Bound where
 transEnv table env uu
  = do	let down :: TransEnv TransEnvType m env a => a -> m a
	    down =  transEnv table env
	
	case uu of
		UVar   v	-> liftM  UVar  (down v)
		UMore  v t	-> liftM2 UMore (down v) (down t)
		UIndex{}	-> return uu
		UClass{}	-> return uu


instance Monad m => TransEnv TransEnvType m env TyCon where
 transEnv = undefined

