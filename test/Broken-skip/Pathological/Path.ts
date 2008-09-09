
import Prelude;
import Tuple;


path0 x
 = { 	a = 0;
 	f = \x -> a := x;
	
	f; 
 };


path2 x
 = {
 	a	= 0;
	n	= 5;
	f	= \x -> a := n;
	
	r	= Tuple2 a f;

	r;
 };

