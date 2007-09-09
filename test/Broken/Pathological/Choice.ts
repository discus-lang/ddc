
import Prelude;

data Sel3 %r
	= S1
	| S2
	| S3;


x1	= 2;
x2	= 3;

print1 ()	= print "hello";
print2 ()	= print "dude";

printX1 ()	= print $ showInt x1;
printX2 ()	= print $ showInt x2;


-----
choose b x y
 = if b 
 	then x 
	else y;
	
chooseI1	= choose True print1 print2;

chooseI2 	= choose True printX1 printX2;


-----
chooseF2 :: forall a %r1 !e1 !e2
	 .  Sel3 %r1 -> (a -(!e1)> a) -> (a -(!e1)> a) -(!e2)> a -(!e1)> a;

chooseF2 b f g
 = case b of {
 	S1	-> f;
	S2	-> g;
	S3	-> \x -> x;
 };

-----
chooseF3 :: forall %r1 !e1 !e2
	 .  Sel3 %r1 -> (Unit -(!e1)> Unit) -(!e2)> Unit -(!e1)> Unit
	 :- !e1 = {Console}
	 ,  !e2 = {Read %r1};

chooseF3 b f
 = case b of {
 	S1	-> f;
	S2	-> \x -> { print "dude"; (); };
	S3	-> \x -> x;
 };


{-
choosef b f x y
	= ( f x
	  , f y 
	  , case b of {
	  	True	-> \z1 z2. z1;
		False	-> x;
	  } );
-}

