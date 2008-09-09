
import Prelude;



init xx
 = match {
 	| Cons x xs	<- xx
	, Cons y ys	<- xs
	= Cons x Nil;
	
	| Cons x xs	<- xx
	= Cons x (init xs);
	
	# error "init: empty list";
 };

