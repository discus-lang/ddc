{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- | Generic transformations on type expressions.
--   This isn't particuarly fast. Depending on what you're doing, it may be better to write
--   traversal code specific to the transform you're implementing.
module DDC.Type.Transform
	( TransM	(..)
	, TransTable	(..)
	, transTableId
	, transZ
	, transformT
	, transformV
	, transformCid
	, transformTM)
where
import DDC.Type.Exp
import DDC.Var
import Util.Data.Bag		(Bag)
import qualified Util.Data.Bag 	as Bag
import qualified Data.Set	as Set
import qualified Data.Map	as Map
import Util


-- | Monadic transforms.
class Monad m => TransM m a 
 where	transZM :: TransTable m -> a -> m a


-- | Apply a transform in the identity monad.
transZ 	:: TransM (State ()) a
	=> TransTable (State ()) -> a -> a
	
transZ	table x 
	= evalState (transZM table x) ()


-- | Table containing fns to apply at each node in the AST.
data TransTable m
	= TransTable
	{ -- | Transform to apply to variables.
	  transV	:: Var  	-> m Var

	  -- | Transform to apply to classids.
	, transCid	:: ClassId	-> m ClassId

	  -- | Transform to apply to type constructors.
	, transC	:: TyCon	-> m TyCon

	  -- | Transform to apply to TProj nodes in a type.
	, transJ	:: TProj 	-> m TProj

	  -- | Transform to apply to fetters in a type.
	, transF	:: Fetter	-> m Fetter

	  -- | Transform to apply to kinds in a type.
	, transK	:: Kind		-> m Kind

	  -- | Transform to apply to types.
	  --   The default one of these is to invoke the enter, follow, and leave fns below.
	, transT	:: TransTable m -> Type -> m Type

	  -- | Transform to apply to types in a top-down manner.
	, transT_enter	:: Type		-> m Type

 	  -- | Transform that applies itself to children of a node in the AST.
	, transT_follow	:: TransTable m -> Type	-> m Type

	  -- | Transform to apply to types in a bottom-up manner.
	, transT_leave	:: Type		-> m Type }


-- | Identity transform that just returns the original type.
transTableId :: Monad m => TransTable m
transTableId
	= TransTable
	{ transV	= \x -> return x
	, transCid	= \x -> return x
	, transC	= \x -> return x
	, transJ	= \x -> return x
	, transF	= \x -> return x
	, transK	= \x -> return x 
	, transT	= transT_default
	, transT_follow	= followT
	, transT_enter	= \t -> return t
	, transT_leave	= \t -> return t }


-- Common Transforms  ----------------------------------------------------------------------------
-- | Apply a transform to all type expressions in a type.
transformT   f t	= transZ  transTableId { transT_leave 	= \x -> return $ f x } t

-- | Apply a monadic transform to all type expressions in a type.
transformTM  f t	= transZM transTableId { transT_leave	= f } t

-- | Apply a transform to all variables in a type.
transformV   f t	= transZ  transTableId { transV 	= \x -> return $ f x } t

-- | Apply a transform to all classids in a type.
transformCid f t	= transZ  transTableId { transCid	= \x -> return $ f x } t


-- Basic Types ------------------------------------------------------------------------------------
instance (Monad m, TransM m a) 
	=> TransM m [a] where
 transZM table xx	
 	 = mapM (transZM table) xx
 

instance (Monad m, TransM m a)
	=> TransM m (Bag a) where
 transZM table xx
 	= liftM Bag.fromList
	$ (transZM table)
	$ Bag.toList xx


instance (Monad m, TransM m a, Ord a)
	=> TransM m (Set a) where
 transZM table xx
 	= liftM Set.fromList
	$ transZM table
	$ Set.toList xx


instance (Monad m, TransM m a, TransM m b) 
 	=> TransM m (a, b) where
 transZM table (a, b)
  = liftM2 (,) (transZM table a) (transZM table b)


instance (Monad m, TransM m a, TransM m b, TransM m c)
	=> TransM m (a, b, c) where
 transZM table (a, b, c)
  = liftM3 (,,) (transZM table a) (transZM table b) (transZM table c)

 
-- Var / ClassId -----------------------------------------------------------------------------------
instance Monad m => TransM m Var
 where	transZM	= transV

instance Monad m => TransM m ClassId
 where	transZM = transCid


-- Kind --------------------------------------------------------------------------------------------
instance Monad m => TransM m Kind where
  transZM table kk
   = case kk of
 	KFun k1 k2	
 	 ->  liftM2 KFun (transZM table k1) (transZM table k2)
	 >>= transK table 

	_ -> transK table kk
	

-- Type --------------------------------------------------------------------------------------------
instance Monad m => TransM m Bind where
 transZM table tt
  = case tt of
	BNil 		-> return BNil
  	BVar v		-> liftM  BVar (transZM table v)
	BMore v t	-> liftM2 BMore (transZM table v) (transZM table t)

instance Monad m => TransM m Type where
 transZM table tt 
  = transT table table tt
  
transT_default table tt
 = do	tE	<- transT_enter	table tt
 	tF	<- transT_follow table table tE
	tL	<- transT_leave table tF
	return	tL

followT table tt
   = case tt of
	TNil		-> return tt
	TForall b k t	-> liftM3 TForall (transZM table b) (transZM table k) (transZM table t)
	TFetters t fs	-> liftM2 TFetters (transZM table t) (transZM table fs)

	TConstrain t Constraints { crsEq, crsMore, crsOther }
	 -> do	t'		<- transZM table t
		crsEq'		<- liftM Map.fromList $ transZM table $ Map.toList crsEq
		crsMore'	<- liftM Map.fromList $ transZM table $ Map.toList crsMore
		crsOther'	<- transZM table crsOther
		return	$ TConstrain t' (Constraints crsEq' crsMore' crsOther')
	
	TSum k ts	-> liftM2 TSum    (return k) (transZM table ts)
 	TVar k u	-> liftM2 TVar    (return k) (transZM table u)
	TApp t1 t2	-> liftM2 TApp    (transZM table t1) (transZM table t2)
	TCon tycon	-> liftM  TCon    (transZM table tycon)
	TError _ _	-> return tt


-- Bound -------------------------------------------------------------------------------------------
instance Monad m => TransM m Bound where
 transZM table uu
  = case uu of
 	UVar v		-> liftM  UVar	(transZM table v)
	UMore v t	-> liftM2 UMore	(transZM table v) (transZM table t)
	UIndex{}	-> return uu
	UClass cid	-> liftM  UClass (transZM table cid)


-- TyCon -------------------------------------------------------------------------------------------
instance Monad m => TransM m TyCon where
 transZM table tt
  = case tt of
  	TyConFun{}		-> return tt
		
	TyConData { tyConName }
	 -> do	v'	<- transZM table tyConName
	 	return	$ tt { tyConName = v' }

	TyConEffect 	{ tyConEffect  = TyConEffectTop v }
	 -> do	v'	<- transZM table v
		return	$ tt { tyConEffect = TyConEffectTop v' }
	
	TyConEffect{}		-> return tt

	TyConClosure (TyConClosureFree v) kind
	 -> do	v'	<- transZM table v
		return	$ TyConClosure (TyConClosureFree v') kind
		
	TyConClosure{}		-> return tt
		
	TyConWitness 	{ tyConWitness = TyConWitnessMkVar v }
	 -> do	v'	<- transZM table v
	 	return	$ tt { tyConWitness = TyConWitnessMkVar v' }

	TyConWitness {}		-> return tt
	TyConElaborate{}	-> return tt

	
-- TProj -------------------------------------------------------------------------------------------
instance Monad m => TransM m TProj where 
  transZM table jj
   = case jj of
 	TJField v	-> liftM TJField (transZM table v) 	>>= transJ table
	TJFieldR v	-> liftM TJFieldR (transZM table v)	>>= transJ table
	TJIndex _	-> transJ table jj
	TJIndexR _	-> transJ table jj


-- Fetter ------------------------------------------------------------------------------------------
instance Monad m => TransM m Fetter where	
  transZM table ff
   = case ff of
	FConstraint v ts	
	 ->  liftM2 FConstraint (transZM table v) (transZM table ts)
	 >>= transF table
						
	FWhere t1 t2		
	 ->  liftM2 FWhere (transZM table t1) (transZM table t2)
	 >>= transF table
	
	FMore t1 t2
	 -> liftM2 FMore (transZM table t1) (transZM table t2)
	 >>= transF table
		
	FProj pj v tDict tBind
	 -> liftM4 FProj (return pj) (transZM table v) 
			 (transZM table tDict) (transZM table tBind)
	 >>= transF table
				
	