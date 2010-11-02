{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DDC.Type.TransEnv
	( module DDC.Util.Plate.TransEnv
	, TransEnvType(..)
	, transEnvTypeId)
where
import DDC.Util.Plate.TransEnv
import DDC.Type
import DDC.Var
import Control.Monad
import qualified Data.Map	as Map


-- | Table containing the functions we need to transEnvform type expressions.
data TransEnvType m env
	= TransEnvType
	{ transEnvTypeV		:: TransEnvUp   m env Var

	, transEnvTypeT_down	:: TransEnvDown m env Type
	, transEnvTypeT_up	:: TransEnvUp   m env Type

	, transEnvTypeK_down	:: TransEnvDown m env Kind
	, transEnvTypeK_up	:: TransEnvUp   m env Kind }
	

-- | Identity transformation table.
transEnvTypeId :: Monad m => TransEnvType m env
transEnvTypeId
	= TransEnvType
	{ transEnvTypeV		= transEnvUpId

	, transEnvTypeT_down	= transEnvDownId
	, transEnvTypeT_up	= transEnvUpId

	, transEnvTypeK_down	= transEnvDownId
	, transEnvTypeK_up	= transEnvUpId }


-- Instances ------------------------------------------------------------------

-- Super
instance Monad m => TransEnv TransEnvType m env Super where
 transEnv table env ss
  = do	let down :: TransEnv TransEnvType m env a => a -> m a
	    down =  transEnv table env

	case ss of
		SProp		-> return ss
		SBox		-> return ss
		SFun k s	-> liftM2 SFun (down k) (down s)

-- Kind
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

-- Type
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
	
-- Constraints	
instance Monad m => TransEnv TransEnvType m env Constraints where
 transEnv table env (Constraints crsEq crsMore crsOther)
  = do	let down :: TransEnv TransEnvType m env a => a -> m a
	    down =  transEnv table env
	
	crsEq'		<- liftM Map.fromList $ down $ Map.toList crsEq
	crsMore'	<- liftM Map.fromList $ down $ Map.toList crsMore
	crsOther'	<- down crsOther
	
	return	$ Constraints crsEq' crsMore' crsOther'

-- Fetter
instance Monad m => TransEnv TransEnvType m env Fetter where
 transEnv table env ff
  = do	let down :: TransEnv TransEnvType m env a => a -> m a
	    down =  transEnv table env
	
	case ff of
		FConstraint v ts -> liftM2 FConstraint (down v)  (down ts)
		FWhere t1 t2	 -> liftM2 FWhere      (down t1) (down t2)
		FMore  t1 t2	 -> liftM2 FMore       (down t1) (down t2)
		FProj  j v t1 t2 -> liftM4 FProj       (down j)  (down v) (down t1) (down t2)

-- TProj
instance Monad m => TransEnv TransEnvType m env TProj where
 transEnv table env jj
  = do	let down :: TransEnv TransEnvType m env a => a -> m a
	    down =  transEnv table env

	case jj of
		TJField  v	-> liftM TJField  (down v)
		TJFieldR v	-> liftM TJFieldR (down v)
		TJIndex  v	-> liftM TJIndex  (down v)
		TJIndexR v	-> liftM TJIndexR (down v)

-- Bind
instance Monad m => TransEnv TransEnvType m env Bind where
 transEnv table env bb
  = do	let down :: TransEnv TransEnvType m env a => a -> m a
	    down =  transEnv table env
	
	case bb of
		BNil		-> return bb
		BVar  v		-> liftM  BVar  (down v)
		BMore v t	-> liftM2 BMore (down v) (down t)

-- Bound
instance Monad m => TransEnv TransEnvType m env Bound where
 transEnv table env uu
  = do	let down :: TransEnv TransEnvType m env a => a -> m a
	    down =  transEnv table env
	
	case uu of
		UVar   v	-> liftM  UVar  (down v)
		UMore  v t	-> liftM2 UMore (down v) (down t)
		UIndex{}	-> return uu
		UClass{}	-> return uu

-- TyCon
instance Monad m => TransEnv TransEnvType m env TyCon where
 transEnv table env tc
  = do	let down :: TransEnv TransEnvType m env a => a -> m a
	    down =  transEnv table env
	
	case tc of
		TyConFun		-> return tc
		TyConData      v k def	-> liftM3 TyConData      (down v)    (down k) (return def)
		TyConEffect    te k	-> liftM2 TyConEffect    (down te)   (down k)
		TyConClosure   tx k	-> liftM2 TyConClosure   (down tx)   (down k)
		TyConWitness   tw k     -> liftM2 TyConWitness   (down tw)   (down k)
		TyConElaborate te k     -> liftM2 TyConElaborate (return te) (down k)
	
-- TyConEffect
instance Monad m => TransEnv TransEnvType m env TyConEffect where
 transEnv table env tc
  = do	let down :: TransEnv TransEnvType m env a => a -> m a
	    down =  transEnv table env
	
	case tc of
		TyConEffectTop v	-> liftM TyConEffectTop (down v)
		_			-> return tc
		
-- TyConClosure
instance Monad m => TransEnv TransEnvType m env TyConClosure where
 transEnv table env tc
  = do	let down :: TransEnv TransEnvType m env a => a -> m a
	    down =  transEnv table env
	
	case tc of
		TyConClosureFreeType v	 -> liftM TyConClosureFreeType   (down v)
		TyConClosureFreeRegion v -> liftM TyConClosureFreeRegion (down v)
		TyConClosureFree v	 -> liftM TyConClosureFree       (down v)
		_			 -> return tc


-- TyConWitness
instance Monad m => TransEnv TransEnvType m env TyConWitness where
 transEnv table env tc
  = do	let down :: TransEnv TransEnvType m env a => a -> m a
	    down =  transEnv table env
	
	case tc of
		TyConWitnessMkVar v	-> liftM TyConWitnessMkVar (down v)
		_			-> return tc

-- Var
instance Monad m => TransEnv TransEnvType m env Var where
 transEnv table env v 	= transEnvTypeV table env v

