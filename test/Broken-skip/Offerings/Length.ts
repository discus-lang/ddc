
import Prelude;



-----
length1 xx
 = case xx of {
 	Nil		
	 -> 0;

	Cons x xs	
	 -> 1 + length1 xx;
 };



-----
length2' xx acc
 = case xx of {
 	Nil		
	 -> acc;

	Cons x xs	
	 -> length2' xs (acc + 1);
 };

length2 xx
	= length2' xx 0;




-----
length3 xx
 = {
 	count	= 0;
	walk xx (\x. count <> inc);
	
	count;
 };	
			


{-		
-----
length3 xx
	count 	= 0
	maps_ xx (\x. count <> inc)
	count
	
 


	 		
-}	
	
