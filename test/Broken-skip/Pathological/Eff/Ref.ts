
import Prelude;


effect Effect1;
effect Effect2;

import extern 
{
	doE1 	:: forall %r1 %r2. Unit{%r1} -{Effect1}> Unit{%r2};
	doE2	:: forall %r1 %r2. Unit{%r1} -{Effect2}> Unit{%r2};
}

updateRefF r f
 = {
 	a	= case True of {
			True	-> f;
			False	-> \x. x;
		};
		
	refSet r f;
 };


main () = {

	r	= refNew_ ();
		
--	refSet r doE1;
--	refSet r doE2;
	
	updateRefF r doE1;
	
	r;
};

