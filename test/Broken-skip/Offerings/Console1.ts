
import Prelude;

traumaMain 
 = {
	list5	= [1, 2, 3, 4, 5];
	list23	= intRange 1 23;

 	printString 
		$ concats 
		[ "list5                 = " % showListInt list5 			% "\n"
		, "list23                = " % showListInt (map (\x. x * 2) list23)	% "\n"
		, "sum list23            = " % showInt (sum list23)			% "\n" 
		, "list5 ++ list23       = " % showListInt (list5 ++ list23)		% "\n"
		, "rev (list5 ++ list32) = " % showListInt (reverse $ list5 ++ list23)	% "\n" ];


 };

