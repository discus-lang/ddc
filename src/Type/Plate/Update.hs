
module Type.Plate.Update
(
--	updateT,
--	updateE,
--	updateF,
--	updateR
)

where

-----
import	Util
import	Shared.Error

import	qualified Shared.Var	as Var
import	Shared.Var		(NameSpace(..))

import	Type.Exp
import	Type.Plate.Trans
--import	Type.Squid.Class

-----
stage	= "Type.Plate.Update"

{-
-----------------------
-- updateT
--	updateT is like a "shallow" sink. 
--	It sinks variable _names_, but does not actually substitute types.
--	
type Update x
	=  Monad m
	=> (?sinkVar 		:: Var -> m Var)
	-> (?sinkClassId	:: ClassId -> m ClassId)
	-> x -> m x

table	= transTableId 
	{ transV	= sinkVar
	, transCid	= sinkClassId }

updateT 	:: Update Type
updateT t 	= transZM table t

updateE		:: Update Effect
updateE e	= transZM table e

updateF		:: Update Fetter
updateF	f	= transZM table f

updateR		:: Update Region
updateR r	= transZM table r
-}
	
	

{-

updateT		t
 = case t of
 	TVar v 	
	 -> do 	v'	<- ?sinkVar v
		return	$ TVar v' 
		
	TFun t1 t2 eff clo
	 -> do 	t1'	<- updateT t1
		t2'	<- updateT t2
		eff'	<- updateE eff
		clo'	<- updateC clo
		
		return	$ TFun t1' t2' eff' clo'

	TCon v ts	
	 -> do	ts'	<- mapM updateT ts
		return	$ TCon v ts'

	TForall vks t	
	 -> do	vks'	<- mapM updateVK vks
	 	t'	<- updateT t
		return	$ TForall vks' t'


	TProj t j
	 -> do	t'	<- updateT t
	 	return	$ TProj t' j	
		
	TRegion r
	 -> do 	r'	<- updateR r
		return	$ TRegion r'

	TEffect e
	 -> do	e'	<- updateE e
	 	return	$ TEffect e'

	TClosure c
	 -> do	c'	<- updateC c
	 	return	$ TClosure c'

	TFetters fs t
	 -> do
	 	fs'	<- mapM updateF fs
		t'	<- updateT t
		return	$ TFetters fs' t'

	TClass cidT
	 -> do	cidT'	<- ?sinkClassId cidT
		return	$ TClass cidT'
	
	TFetter f
	 -> do
	 	f'	<- updateF f
		return	$ TFetter f'

	TError
	 ->	return	$ TError

	_ -> panic stage 
		$ "updateT: no match for " % show t

updateVK ::	Update (Var, Kind)
updateVK	(v, k)
 = do
 	v'	<- ?sinkVar v
	return	$ (v', k)

-----
updateF ::	Update Fetter
updateF		f
 = case f of
	FClass v ts
	 -> do	ts'	<- mapM updateT ts
	 	return	$ FClass v ts'

 	FEffect e eff
	 -> do 	e'	<- updateE e
		eff'	<- updateE eff
		return	$  FEffect e' eff'
	
	FFieldHas t1 t2 proj
	 -> do	t1'	<- updateT t1
	 	t2'	<- updateT t2
		return	$ FFieldHas t1' t2' proj
		
	FFieldIs t1 t2 proj
	 -> do	t1'	<- updateT t1
	 	t2'	<- updateT t2
		return	$ FFieldIs t1' t2' proj		

-----
updateE ::	Update Effect
updateE		e
 = case e of
	ENil
	 -> 	return	$ ENil

	EVar v 		
	 -> do	v'	<- ?sinkVar v
		return	$  EVar v'

	ECon v ts
	 -> do 	ts'	<- mapM updateT ts
		return	$ ECon v ts'

	ESum es
	 -> do 	es'	<- mapM updateE es
		return	$ ESum (nub es')

	EClass cidE
	 -> do	cidE'	<- ?sinkClassId cidE
	 	return	$ EClass cidE'
	
	EFetters fs e
	 -> do	fs'	<- mapM updateF fs
	 	e'	<- updateE e
		return	$ EFetters fs' e'

-----
updateC ::	Update Closure
updateC		c
 =	transZM 
 		transTableId {
			trans

 = transformCM 

 = case c of
 	CNil 
	 ->	return CNil
	 
		
				

-----
updateR ::	Update Region
updateR r
 = case r of
 	RVar v
	 -> do	v'	<- ?sinkVar v
		return	$ RVar v'

	RClass cidR
	 -> do 	cidR'	<- ?sinkClassId cidR
	 	return	$ RClass cidR'

-----
updateC ::	Update Closure
updateC c 
 = case c of
 	CNil	-> return CNil
	
	CVar v
	 -> do	v'	<- ?sinkVar v 
	 	return	$ CVar v'
		
	CSum cs
	 -> do	cs'	<- mapM updateC cs
	 	return	$ CSum cs'
		
	CFree v t
	 -> do	t'	<- updateT t
	 	return	$ CFree v t'
		
	CImplicit v t
	 -> do	t'	<- updateT t
	 	return	$ CImplicit v t'
		
		
-}



