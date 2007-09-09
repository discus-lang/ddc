-- EffectPort.ts
--	Tests out summing of effects via ports.
--	
--	A higher order function can end up containing multiple effect ports.
--	Any concrete effects unified with this function need to be sent though
--	both ports.
--			
import Prelude;
import Tuple;

-----
fun1 	::  forall %r1 %r2
        .    Bool{%r1} -> (a -{!e0}> a) -> c 
	-A> Tuple2{%r2} (a -{!e0}> a) c

	:-  A = {Read %r1};

fun1 	b g1 c	= 
{
	r	= case b of {
			True 	-> g1;
			False	-> \x. x;
		};
				
	Tuple2 g1 c;
};


-----
fun2 	::  forall %r1 %r2
	.   Bool{%r1} -> (a -{!e1}> a) -> c 
	-A> Tuple2{%r2} c (a -{!e1}> a)

	:-  A = {Read %r1};

fun2	b       g1 		 c	= 
{
	r	= case b of {
			True	-> g1;
			False	-> \x. x;
		};
		
	Tuple2 	c g1;
};


-----
fun3 	::  forall %r1 %r2 %r3
	.   Bool{%r1} 
	-A> Bool{%r2} -> (a -{!e0, !e1}> a) -> (a -{!e0, !e1}> a)
	-B> Tuple2{%r3} (a -{!e0}> a) (a -{!e1}> a)
	
	:- A = {Read %r1}
	,  B = {Read %r2};

fun3	b	
 = case b of {
 	True	-> fun1;
	False	-> fun2;
 };


{-
fun4 b x
 = case b of {
 	True	-> fun1;
	False	-> fun2;
	False	-> x;
 };


-----
effect Effect1;
effect Effect2;

import extern 
{
	doE1 	:: forall %r1 %r2. Unit{%r1} -{Effect1}> Unit{%r2};
	doE2	:: forall %r1 %r2. Unit{%r1} -{Effect2}> Unit{%r2};
}


result	:: Tuple2 (() -A> ()) (() -A> ())
	:- A = {Effect1, Effect2};

result	= fun3 True True doE1 doE2;

-}
