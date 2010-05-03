
-- | DeepSeq on Type expressions.
module Type.Plate.DeepSeq where

import Type.Exp
import DDC.Var
import Util.Control.DeepSeq

instance DeepSeq Var
instance DeepSeq ClassId

instance DeepSeq Super where
 deepSeq xx y
  = case xx of
	SProp				-> y
	SBox 				-> y
	SFun 		s1 s2		-> deepSeq s1 $! deepSeq s2 y
	
 
instance DeepSeq Kind where
 deepSeq xx y
  = case xx of
	KNil				-> y
	KCon 		c  s		-> deepSeq c  $! deepSeq s  y
	KFun		k1 k2		-> deepSeq k1 $! deepSeq k2 y
	KApp		k1 t2		-> deepSeq k1 $! deepSeq t2 y
	KWitJoin 	ks		-> deepSeq ks y


instance DeepSeq KiCon where
 deepSeq xx y
  = case xx of
	KiConVar 	v		-> deepSeq v y
	_				-> y

instance DeepSeq Bind where
 deepSeq xx y
  = case xx of
	BVar 		v		-> deepSeq v y
	BMore 		v t		-> deepSeq v $! deepSeq t y


instance DeepSeq Type where
 deepSeq xx y
  = case xx of
	TNil				-> y
	TForall		b k t		-> deepSeq b  $! deepSeq k $! deepSeq t y
	TContext	k t		-> deepSeq k  $! deepSeq t y
	TFetters	t fs		-> deepSeq t  $! deepSeq fs y
	TApp		t1 t2		-> deepSeq t1 $! deepSeq t2 y
	TSum		k ts		-> deepSeq k  $! deepSeq ts y
	TCon		c		-> deepSeq c y
	TVar		k v		-> deepSeq k  $! deepSeq v y
	TTop		k		-> deepSeq k y
	TBot		k		-> deepSeq k y
	TEffect		v ts		-> deepSeq v  $! deepSeq ts y
	TFree		v t		-> deepSeq v  $! deepSeq t y
	TDanger		t1 t2		-> deepSeq t1 $! deepSeq t2 y
	TElaborate	k s		-> deepSeq k  $! deepSeq s y
	TClass		k c		-> deepSeq k  $! deepSeq c y
	TError		k err		-> deepSeq k y
	TVarMore	k v t		-> deepSeq k  $! deepSeq v $! deepSeq t y
	TIndex		k i 		-> deepSeq k  $! deepSeq i y
	TWitJoin	ws		-> deepSeq ws y


instance DeepSeq Elaboration


instance DeepSeq TyCon where
 deepSeq xx y
  = case xx of
	TyConFun 			-> y
	TyConData 	n k		-> deepSeq n $! deepSeq k y
	TyConWitness 	c k		-> deepSeq c $! deepSeq k y
	

instance DeepSeq TyConWitness where
 deepSeq xx y
  = case xx of
	TyConWitnessMkVar v		-> deepSeq v y
	_				-> y


instance DeepSeq Fetter where
 deepSeq xx y
  = case xx of
	FConstraint 	v ts		-> deepSeq v  $! deepSeq ts y
	FWhere 		t1 t2		-> deepSeq t1 $! deepSeq t2 y
	FMore  		t1 t2		-> deepSeq t1 $! deepSeq t2 y
	FProj  		j v t1 t2	-> deepSeq j  $! deepSeq v $! deepSeq t1 $! deepSeq t2 y


instance DeepSeq TProj where
 deepSeq xx y
  = case xx of
	TJField 	v		-> deepSeq v y
	TJFieldR 	v		-> deepSeq v y
	TJIndex  	v		-> deepSeq v y
	TJIndexR 	v		-> deepSeq v y
	
