
import Prelude;

-----
data Point2
	= Point2 Int Int;

movePoint p xd yd
 = case p of {
 	Point2 x y -> Point2 (x + xd) (y + yd);
 };

	
-----
data Beast
	= Beast
		String			-- name
		Point2			-- pos
		Point2			-- vel
		Int			-- health
		(List Item);		-- inventory

moveBeast b xd yd
 = case b of {
 	Beast name pos vel health inv
	 -> Beast name (movePoint pos xd yd) vel health inv;
 };


-----
data Item
	= Flashlight
	| CRation
	| Knife
	| Box (List Item);
	
