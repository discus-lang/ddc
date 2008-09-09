
import Prelude;


fun1 b f
 = {
 	r	= case b of {
			True	-> f;
			False	-> \x -> x;
		};
		
	Ref f;
 };



fun2 b f
 = {
 	r	= case b of {
			True	-> f;
			False	-> fun1;
		};
		
	Ref f;
 };
