
import Prelude;
import Tuple;



choice b x y	
 = case b of {
 	True	-> x;
	False	-> y;
 };
-}

-----

{-
fun1Choice	= choice True fun1 fun1;

fun1Sel		
 = case True of	{
 	True	-> fun1;
	False	-> fun1;
 };
-}



fun2 y	= asType y fun1;
