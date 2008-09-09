
import	Prelude;
import	Tuple;


x	= Tuple2 1 2;
x2	= copy x;

f1 x	= Tuple2 a 3;
f1c	= copy f1;


box1	= Ref f1;
box1c	= copy box1;

