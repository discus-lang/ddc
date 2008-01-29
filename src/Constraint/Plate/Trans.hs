
module Constraint.Plate.Trans
	( transM 
	, Table (..))
	
where

import Type.Exp
import Constraint.Exp
import Constraint.Pretty
import Shared.Error
import Util

-----
stage	= "Constraint.Plate.Trans"


-- | Transform class
class Monad m => TransM m a where
 transM :: Table m -> a -> m a

-- | Transform table
data Table m
	= Table
	{ tableTransT	:: Type -> m Type }
	
-- Simple Instances --------------------------------------------------------------------------------
instance (Monad m, TransM m a) => TransM m [a] where
 transM table []	= return []
 transM table (x:xs)	
  = do	x'	<- transM table x
  	xs'	<- mapM (transM table) xs
	return	$  x' : xs'


-- CTree -------------------------------------------------------------------------------------------
instance Monad m => TransM m CTree where
 transM table xx 
  = let down xx = transM table xx
    in case xx of
  	CTreeNil	-> return CTreeNil
	
	CBranch bb bs
	 -> do 	bs'	<- down bs
		return	$ CBranch bb bs'
	
	CDef ts t1 t2
	 -> do 	t1'	<- down t1
		t2'	<- down t2
		return	$ CDef ts t1' t2'

	CSig ts t1 t2
	 -> do 	t1'	<- down t1
		t2'	<- down t2
		return	$ CSig ts t1' t2'

	CEq ts t1 t2
	 -> do 	t1'	<- down t1
		t2'	<- down t2
		return	$ CEq ts t1' t2'
	
	CEqs ts tt
	 -> do 	tt'	<- down tt
		return	$ CEqs ts tt'
		
	CClass ts v tt
	 -> do 	tt'	<- down tt
		return	$ CClass ts v tt'
		
	CProject ts j v t1 t2
	 -> do 	t1'	<- down t1
		t2'	<- down t2
		return	$ CProject ts j v t1' t2'
		
	CInst{}	-> return xx
	
	CGen ts t
	 -> do	t'	<- down t
	 	return	$ CGen ts t'
		
	CDataFields ss v1 vs vts
	 -> do	let (vs, ts)	= unzip vts
	 	ts'		<- down ts
		let vts'	= zip vs ts'
		return	$ CDataFields ss v1 vs vts'

	CDictProject ts t mv
	 -> do	t'	<- down t
	 	return	$ CDictProject ts t' mv
	
	CClassInst ts v tt
	 -> do	tt'	<- down tt
	 	return	$ CClassInst ts v tt'
		
	-- don't handle constructors used internally by the solver
	_ -> panic stage
		$ "transM/CTree: no match for " % xx % "\n"

-- Type --------------------------------------------------------------------------------------------
instance Monad m => TransM m Type where
 transM table t
  = do	t'	<- tableTransT table t
  	return	t'

