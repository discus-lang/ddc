{-# OPTIONS -fwarn-incomplete-patterns #-}

-- | Generic transformations on type expressions.
module Type.Plate.Trans
	( TransM(..)
	, TransTable (..)
	, transTableId
	, transZ
	, transformT
	, transformV
	, transformCid
	, transformTM)
where
import Util
import Type.Exp
import DDC.Var
import Util.Data.Bag		(Bag)
import qualified Util.Data.Bag 	as Bag
import qualified Data.Set	as Set
import qualified Data.Map	as Map


-----
class Monad m => TransM m a 
 where	transZM :: TransTable m -> a -> m a

-----
transZ 
	:: TransM (State ()) a
	=> TransTable (State ())
	-> a -> a
	
transZ	table x 
	= evalState (transZM table x) ()


-----
data TransTable m
	= TransTable
	{ transV	:: Var  	-> m Var
	, transCid	:: ClassId	-> m ClassId

	-- type
	, transT	:: TransTable m -> Type   	-> m Type
	, transT_follow	:: TransTable m -> Type		-> m Type
	, transT_enter	:: Type		-> m Type
	, transT_leave	:: Type		-> m Type

	, transC	:: TyCon	-> m TyCon
	, transJ	:: TProj 	-> m TProj
	, transF	:: Fetter	-> m Fetter
	, transK	:: Kind		-> m Kind }

-----
transTableId 
	:: Monad m
	=> TransTable m
	
transTableId
	= TransTable
	{ transV	= \x -> return x
	, transCid	= \x -> return x

	-- type
	, transT	= transT_default
	, transT_follow	= followT
	, transT_enter	= \t -> return t
	, transT_leave	= \t -> return t

	, transC	= \x -> return x
	, transJ	= \x -> return x
	, transF	= \x -> return x
	, transK	= \x -> return x }

-----
transformT f t		= transZ  transTableId { transT_leave = \x -> return $ f x } t
transformV f t		= transZ  transTableId { transV = \x -> return $ f x } t
transformCid f t	= transZ  transTableId { transCid = \x -> return $ f x } t
		
transformTM f t	= transZM transTableId { transT_leave = f } t

-----
instance (Monad m, TransM m a) 
		=> TransM m [a]
 where	transZM table xx	
 	 = 	mapM (transZM table) xx
 
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
  = do
	a'	<- transZM table a
	b'	<- transZM table b
	return	(a', b')


instance (Monad m, TransM m a, TransM m b, TransM m c)
		=> TransM m (a, b, c) where
 transZM table (a, b, c)
  = do	
 	a' 	<- transZM table a
	b'	<- transZM table b
	c'	<- transZM table c
	return	(a', b', c')

 
-- Var / ClassId -----------------------------------------------------------------------------------
instance Monad m => TransM m Var
 where	transZM	= transV

instance Monad m => TransM m ClassId
 where	transZM = transCid


-- Type --------------------------------------------------------------------------------------------
instance Monad m => TransM m Bind where
 transZM table tt
  = case tt of
  	BVar v
	 -> do	v'	<- transZM table v
	 	return	$ BVar v'
		
	BMore v t
	 -> do	v'	<- transZM table v
	 	t'	<- transZM table t
		return	$ BMore v' t'


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
	TNil
	 -> do	return	$ TNil

	TForall b k t
	 -> do	b'	<- transZM table b
	 	k'	<- transZM table k
	 	t'	<- transZM table t
		return	$ TForall b' k' t'

	TContext k t
	 -> do	k'	<- transZM table k
	 	t'	<- transZM table t
		return	$ TContext k' t'

	TFetters t fs
	 -> do	t'	<- transZM table t
	 	fs'	<- transZM table fs
		return	$ TFetters t' fs'

	TConstrain t cs@Constraints { crsEq, crsMore, crsOther }
	 -> do	t'		<- transZM table t
		crsEq'		<- liftM Map.fromList $ transZM table $ Map.toList crsEq
		crsMore'	<- liftM Map.fromList $ transZM table $ Map.toList crsMore
		crsOther'	<- transZM table crsOther
		
		return	$ TConstrain t' (Constraints crsEq' crsMore' crsOther')
	
	TSum k ts
	 -> do	ts'	<- transZM table ts
	 	return	$ TSum k ts'

 	TVar k v
	 -> do	v'	<- transZM table v
	 	return	$ TVar k v'

	TApp t1 t2
	 -> do	t1'	<- transZM table t1
	 	t2'	<- transZM table t2
		return	$ TApp t1' t2'
		
	TCon tycon
	 -> do	tycon'	<- transZM table tycon
	 	return	$ TCon tycon'

	TTop k
	 -> do	return	$ tt
	 
	TBot k
	 -> do	return	$ tt

	-- effect
	TEffect v ts
	 -> do	v'	<- transZM table v
	 	ts'	<- transZM table ts
		return	$ TEffect v' ts'

	-- closure
	TFree v t
	 -> do	v'	<- transZM table v
		t'	<- transZM table t
	 	return	$ TFree v' t'

	TDanger v t
	 -> do	v'	<- transZM table v
	 	t'	<- transZM table t
		return	$ TDanger v' t'

	-- used in solver
	TClass k cid
	 -> do	cid'	<- transZM table cid
	 	return	$ TClass k cid'

	TError k ts
	 -> do	return tt
	
	-- type sugar
	TElaborate ee t
	 -> do	t'	<- transZM table t
	 	return	$ TElaborate ee t'
		
	TVarMore k v t
	 -> do	k'	<- transZM table k
	 	v'	<- transZM table v
		t'	<- transZM table t
		return	$ TVarMore k' v' t'

	TWitJoin ts
	 -> do	ts'	<- transZM table ts
	 	return	$ TWitJoin ts'

	TIndex{}	-> return tt
				
-- TyCon -------------------------------------------------------------------------------------------
instance Monad m => TransM m TyCon where
 transZM table tt
  = case tt of
  	TyConFun{}
	 -> do	return tt
		
	TyConData { tyConName }
	 -> do	v'	<- transZM table tyConName
	 	return	$ tt { tyConName = v' }
	
	TyConWitness 	{ tyConWitness = TyConWitnessMkVar v }
	 -> do	v'	<- transZM table v

	 	return	$ tt { tyConWitness = TyConWitnessMkVar v' }

	TyConWitness {}
	 -> 	return tt

	
-- TProj -------------------------------------------------------------------------------------------
instance Monad m => TransM m TProj where 
  transZM table jj
   = case jj of
 	TJField v
	 -> do	v'		<- transZM table v
	 	transJ table	$ TJField v'
		
	TJFieldR v
	 -> do	v'		<- transZM table v
	 	transJ table	$ TJFieldR v'

	TJIndex _
	 ->	transJ table	jj
	 
	TJIndexR _
	 ->	transJ table 	jj		


-- Fetter ------------------------------------------------------------------------------------------
instance Monad m => TransM m Fetter where	
  transZM table ff
   = case ff of
	FConstraint v ts
	 -> do	v'		<- transZM table v
	 	ts'		<- transZM table ts
		transF table	$ FConstraint v' ts'
		
	FWhere t1 t2
	 -> do	t1'		<- transZM table t1
	 	t2'		<- transZM table t2
		transF table	$ FWhere t1' t2'

	FMore t1 t2
	 -> do	t1'		<- transZM table t1
	 	t2'		<- transZM table t2
		transF table	$ FMore t1' t2'
	
	FProj pj v tDict tBind
	 -> do	v'		<- transZM table v
	 	tDict'		<- transZM table tDict
		tBind'		<- transZM table tBind
		transF table	$ FProj pj v' tDict' tBind'
	
	
-- Kind --------------------------------------------------------------------------------------------
instance Monad m => TransM m Kind where
  transZM table kk
   = case kk of
 	KFun k1 k2
	 -> do	k1'		<- transZM table k1
	 	k2'		<- transZM table k2
		transK table	$ KFun k1' k2'
		
	_	-> transK table kk
	

