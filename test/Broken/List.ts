
import Base;

data List a
	= Nil
	| Cons a (List a);


map_ f xx
 = case xx of {
 	Nil		-> ();
	Cons x xs
	 -> {
	 	f x;
		map_ f xs; };
 };

maps_ xx f 
 = 	map_ f xx;
