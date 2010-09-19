{-# OPTIONS -no-implicit-prelude #-}

module Class.Floating
import Base

class Floating a where

	ftrunc	:: forall b
		.  a -(!e1 $c1)> b
		:- !e1 = !ReadT a
		,  Shape2 a b

	fround	:: forall b
		.  a -(!e1 $c1)> b
		:- !e1 = !ReadT a
		,  Shape2 a b

	ffloor	:: forall b
		.  a -(!e1 $c1)> b
		:- !e1 = !ReadT a
		,  Shape2 a b

	fmod	:: forall b
		.  a -(!e1 $c1)> b
		:- !e1 = !ReadT a
		,  Shape2 a b

	fpow	:: forall b. 
		.  a -(!e1 $c1)> b
		:- !e1 = !ReadT a
		,  Shape2 a b

	sqrt	:: forall b
		.  a -(!e1 $c1)> b
		:- !e1 = !ReadT a
		,  Shape2 a b

	log	:: forall b
		.  a -(!e1 $c1)> b
		:- !e1 = !ReadT a
		,  Shape2 a b

	sin	:: forall b
		.  a -(!e1 $c1)> b
		:- !e1 = !ReadT a
		,  Shape2 a b

	asin	:: forall b
		.  a -(!e1 $c1)> b
		:- !e1 = !ReadT a
		,  Shape2 a b

	cos	:: forall b
		.  a -(!e1 $c1)> b
		:- !e1 = !ReadT a
		,  Shape2 a b

	acos	:: forall b
		.  a -(!e1 $c1)> b
		:- !e1 = !ReadT a
		,  Shape2 a b

	tan	:: forall b
		.  a -(!e1 $c1)> b
		:- !e1 = !ReadT a
		,  Shape2 a b

	atan	:: forall b
		.  a -(!e1 $c1)> b
		:- !e1 = !ReadT a
		,  Shape2 a b

