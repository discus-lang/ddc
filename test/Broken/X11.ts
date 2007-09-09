
data Display;


import extern {
	xOpenDisplay 	{ "XOpenDisplay" }
		:: Ptr Char -> Ptr Display;


	xDrawLine	{ "XDrawLine" }
		:: (?display 	:: Ptr# Display)
		-> (?drawable 	:: Ptr# Drawable)
		-> (?gc		:: Ptr# GC)
		-> Int# -> Int# -> Int# -> Int#
	
}
