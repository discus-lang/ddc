
module Type.Plate.DeepSeq where

import Type.Exp
import Util.Control.DeepSeq

instance DeepSeq Var
instance DeepSeq ClassId

instance DeepSeq Super where
 deepSeq xx y
  = case xx of
	SCon 		k		-> deepSeq k y
	SFun 		s1 s2		-> deepSeq s1 $ deepSeq s2 y
	

instance DeepSeq KsCon
  	

instance DeepSeq Kind where
 deepSeq xx y
  = case xx of
	KNil				-> y
	KCon 		c  s		-> deepSeq c  $ deepSeq s  y
	KPi  		k1 k2		-> deepSeq k1 $ deepSeq k2 y	
	KForall		k1 k2		-> deepSeq k1 $ deepSeq k2 y
	KFun		k1 k2		-> deepSeq k1 $ deepSeq k2 y
	KValue				-> y
	KRegion				-> y
	KEffect				-> y
	KClosure			-> y
	KWitness			-> y
	KClass		tc ts		-> deepSeq tc $ deepSeq ts y
	KWitJoin 	ks		-> deepSeq ks y


instance DeepSeq KiCon where
 deepSeq xx y
  = case xx of
	KiCon 		v		-> deepSeq v y
	_				-> y

instance DeepSeq Bind where
 deepSeq xx y
  = case xx of
	BVar 		v		-> deepSeq v y
	BMore 		v t		-> deepSeq v $ deepSeq t y


instance DeepSeq Type where
 deepSeq xx y
  = case xx of
	TNil				-> y
	TForall		b k t		-> deepSeq b  $ deepSeq k $ deepSeq t y
	TContext	k t		-> deepSeq k  $ deepSeq t y
	TFetters	t fs		-> deepSeq t  $ deepSeq fs y
	TApp		t1 t2		-> deepSeq t1 $ deepSeq t2 y
	TSum		k ts		-> deepSeq k  $ deepSeq ts y
	TCon		c		-> deepSeq c y
	TVar		k v		-> deepSeq k  $ deepSeq v y
	TTop		k		-> deepSeq k y
	TBot		k		-> deepSeq k y
	TEffect		v ts		-> deepSeq v  $ deepSeq ts y
	TFree		v t		-> deepSeq v  $ deepSeq t y
	TDanger		t1 t2		-> deepSeq t1 $ deepSeq t2 y
	TWild		k		-> deepSeq k y
	TElaborate	k s		-> deepSeq k  $ deepSeq s y
	TData		k v ts		-> deepSeq k  $ deepSeq v $ deepSeq ts y
	TFun		t1 t2 eff clo	-> deepSeq t1 $ deepSeq t2 $ deepSeq eff $ deepSeq clo y
	TClass		k c		-> deepSeq k  $ deepSeq c y
	TError		k ts		-> deepSeq k  $ deepSeq ts y
	TFetter		f		-> deepSeq f y
	TVarMore	k v t		-> deepSeq k  $ deepSeq v $ deepSeq t y
	TIndex		i 		-> deepSeq i y
	TWitJoin	ws		-> deepSeq ws y


instance DeepSeq Elaboration


instance DeepSeq TyCon where
 deepSeq xx y
  = case xx of
	TyConFun 			-> y
	TyConData 	n k		-> deepSeq n $ deepSeq k y
	TyConClass 	c k		-> deepSeq c $ deepSeq k y
	

instance DeepSeq TyClass where
 deepSeq xx y
  = case xx of
	TyClass 	v		-> deepSeq v y
	_				-> y


instance DeepSeq Fetter where
 deepSeq xx y
  = case xx of
	FConstraint 	v ts		-> deepSeq v  $ deepSeq ts y
	FWhere 		t1 t2		-> deepSeq t1 $ deepSeq t2 y
	FMore  		t1 t2		-> deepSeq t1 $ deepSeq t2 y
	FProj  		j v t1 t2	-> deepSeq j  $ deepSeq v $ deepSeq t1 $ deepSeq t2 y


instance DeepSeq TProj where
 deepSeq xx y
  = case xx of
	TJField 	v		-> deepSeq v y
	TJFieldR 	v		-> deepSeq v y
	TJIndex  	v		-> deepSeq v y
	TJIndexR 	v		-> deepSeq v y
	
