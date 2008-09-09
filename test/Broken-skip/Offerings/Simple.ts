
import Prelude;
import List;
import Tuple;


(:) = Cons;

list1	= Cons 1 (Cons 2 (Cons 3 Nil));

-- addX xx	= map (\x -> x + (2 + 3)) xx;


-- snip
--	Testing Core.Snip
--	Normalising core IR so that all applications are between variables.
--
snip1 = {
	a	= 1 + 2 + 3;
 };



snip2 () = {
	a	= 1 
		+ case id True of { 
			True 	-> 2; 
			False 	-> 3; 
		};
 };


snip3 () = {
	a	= 1
		+ let { 
			x = 2; 
			y = 3; 
		  } in (x + y);
 };
	


snip4 = {
	a	= 1 + (\x -> x + 2 * 3) 5;
 };
