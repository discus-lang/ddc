{-# OPTIONS -fwarn-incomplete-patterns #-}

module Type.Plate.Trans
	( TransM(..)
	, TransTable (..)
	, transTableId
	, transZ
	, transformT
	, transformV
	, transformTM)
where

import Util
import Type.Exp

import qualified Util.Data.Bag 	as Bag
import Util.Data.Bag		(Bag)

import qualified Data.Set	as Set
import Data.Set			(Set)

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

	, transJ	= \x -> return x
	, transF	= \x -> return x
	, transK	= \x -> return x }

-----
transformT f t	= transZ  transTableId { transT_leave = \x -> return $ f x } t
transformV f t	= transZ  transTableId { transV = \x -> return $ f x } t
		
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
	$ (transZM table)
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

 
-----------------------
-- Var
--
instance Monad m => TransM m Var
 where	transZM	= transV

instance Monad m => TransM m ClassId
 where	transZM = transCid


-----------------------
-- Type
--
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

	TForall vks t
	 -> do	vks'	<- transZM table vks
	 	t'	<- transZM table t
		return	$ TForall vks' t'

	TFetters fs t
	 -> do	t'	<- transZM table t
	 	fs'	<- transZM table fs
		return	$ TFetters fs' t'

--	TUnify k ts
--	 -> do	ts'	<- transZM table ts
--	 	return	$ TUnify k ts'

	TSum k ts
	 -> do	ts'	<- transZM table ts
	 	return	$ TSum k ts'

	TMask k t ts
	 -> do	k' 	<- transZM table k
	 	t'	<- transZM table t
	 	ts'	<- transZM table ts
		return	$ TMask k t' ts'

 	TVar k v
	 -> do	v'	<- transZM table v
	 	return	$ TVar k v'

	TTop k
	 -> do	return	$ tt
	 
	TBot k
	 -> do	return	$ tt


	-- data
	TFun t1 t2 eff clo
	 -> do	t1'	<- transZM table t1
	 	t2'	<- transZM table t2
		eff'	<- transZM table eff
		clo'	<- transZM table clo
		return	$ TFun t1' t2' eff' clo'

	TData v ts
	 -> do	v'	<- transZM table v
	 	ts'	<- transZM table ts
		return	$ TData v' ts'
		

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

	TTag v
	 -> do	v'	<- transZM table v
	 	return	$ TTag v'

	-- wildcards
	TWild k
	 -> do	k'	<- transZM table k
	 	return	$ TWild k'


	-- used in solver
	TClass k cid
	 -> do	cid'	<- transZM table cid
	 	return	$ TClass k cid'

	TAccept t
	 -> do	t'	<- transZM table t
	 	return	$ TAccept t'

	TFetter f
	 -> do	f'	<- transZM table f
		return	$ TFetter f'

	TError k t
	 -> do	t'	<- transZM table t
	  	return 	$ TError k t'


	TNode cid t
	 -> do	cid'	<- transZM table cid
	 	t'	<- transZM table t
		return	$ TNode cid' t'



	-- type sugar
	TElaborate t
	 -> do	t'	<- transZM table t
	 	return	$ TElaborate t'
		
	TMutable t
	 -> do	t'	<- transZM table t
	 	return	$ TMutable t'
		
	-- extra type constraints
--	TUnify ts
--	 -> do	ts'	<- transZM table ts
--	 	return	$ TUnify ts'

	-- type constraints
	TFunV t1 t2 ml
	 -> do	t1'	<- transZM table t1
	 	t2'	<- transZM table t2
		return	$ TFunV t1' t2' ml 
	
	TFunF xx
	 ->  do	xx'	<- transZM table xx
	 	return	$ TFunF xx

	

	
-----------------------
-- TProj
--
instance Monad m => TransM m TProj
 where 
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


-----------------------
-- Fetter
--
instance Monad m => TransM m Fetter
 where	
  transZM table ff
   = case ff of
	FConstraint v ts
	 -> do	v'		<- transZM table v
	 	ts'		<- transZM table ts
		transF table	$ FConstraint v' ts'
		
	FLet t1 t2
	 -> do	t1'		<- transZM table t1
	 	t2'		<- transZM table t2
		transF table	$ FLet t1' t2'

	FMore t1 t2
	 -> do	t1'		<- transZM table t1
	 	t2'		<- transZM table t2
		transF table	$ FMore t1' t2'
	
	FProj pj v tDict tBind
	 -> do	v'		<- transZM table v
	 	tDict'		<- transZM table tDict
		tBind'		<- transZM table tBind
		transF table	$ FProj pj v' tDict' tBind'
	
	FFunInfo v eff env
	 -> do	eff'		<- transZM table eff
	 	env'		<- transZM table env
		transF table	$ FFunInfo v eff' env'
	
-----------------------
-- Kind
--
instance Monad m => TransM m Kind
 where
  transZM table kk
   = case kk of
 	KFun k1 k2
	 -> do	k1'		<- transZM table k1
	 	k2'		<- transZM table k2
		transK table	$ KFun k1' k2'
		
	KData	 -> transK table kk
	KRegion	 -> transK table kk
	KEffect	 -> transK table kk
	KClosure -> transK table kk
	KFetter  -> transK table kk	

	KNil	-> transK table kk
	KBox	-> transK table kk
	KError	-> transK table kk
	

