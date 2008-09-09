
import Prelude;


fun1 y		= asType y (\x -> x);


fun1ChoiceL f
 = case True of {
 	True	-> fun1;
	False	-> f;
 };


effect Effect1;


fun2 y b	= y b;

dude		= fun1ChoiceL fun2 id ();
