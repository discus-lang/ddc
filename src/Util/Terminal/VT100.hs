
module Util.Terminal.VT100
(
	Mode (..),	codeMode,
	Color (..),	codeColor,
	
	setAttribute,
	setForeground,
	setBackground,

	resetAttribute
)

where

import Data.Maybe

-----
data Mode
	= Reset
	| Bright
	| Dim
	| Underscore
	| Blink
	| Reverse
	| Hidden
	deriving (Show, Eq)


codeMode :: [(Mode, Int)]
codeMode = 
	[ (Reset, 	0)
	, (Bright, 	1)
	, (Dim,		2)
	, (Underscore,	4)
	, (Blink,	5)
	, (Reverse,	7)
	, (Hidden,	8) ]
	
-----
data Color
	= Black
	| Red
	| Green
	| Yellow
	| Blue
	| Magenta
	| Cyan
	| White
	deriving (Show, Eq)
	
	
codeColor :: [(Color, Int)]
codeColor =
	[ (Black,	0)
	, (Red,		1)
	, (Green,	2)
	, (Yellow,	3)
	, (Blue,	4)
	, (Magenta,	5)
	, (Cyan,	6)
	, (White,	7) ]


-----
setAttribute :: Mode -> String
setAttribute	mode
	=  "\27[" 
	++ (show $ fromJust $ lookup mode codeMode)
	++ "m"


setForeground :: Color	-> String
setForeground	 fg
	= "\27["
	++ (show $ (fromJust $ lookup fg codeColor) + 30)
	++ "m"


setBackground :: Color -> String
setBackground 	 bg
	= "\27["
	++ (show $ (fromJust $ lookup bg codeColor) + 40)
	++ "m"
	

resetAttribute :: String
resetAttribute
	= "\27["
	++ (show $ fromJust $ lookup Reset codeMode)
	++ "m"
	
	
	
