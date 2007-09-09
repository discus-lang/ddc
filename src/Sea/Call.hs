
module Sea.Call
(
--	callTree
)

where

import Util
import Sea.Exp
import Sea.Plate.Trans

{-
callTree ::	Tree -> Tree
callTree	tree
	= tree


 	= transformSS callSS tree
	
	
callSS ::	[Stmt] -> [Stmt]
callSS	ss
 = catMap callS ss
 
 
callS ::	Stmt	-> [Stmt]
callS	s
 = case s of
 	SStmt (XCall v xx)	
	 -> let
	 	(boxedArgs, unboxedArgs)	
			= partition (\x -> x =@= XSlot{} 
					|| x =@= XSlotA{})
				xx

	    in	[ SComment $ "Call " ++ show v ]
	     ++ (map SSlotPush boxedArgs)
	     ++ [ SStmt (XCall v unboxedArgs)
	        , SSlotPop (length boxedArgs) 
		, SBlank ]
	     

	SAssign x t (XCall v xx)
	 -> let
	 	(boxedArgs, unboxedArgs)	
			= partition (\x -> x =@= XSlot{}
					|| x =@= XSlotA{})
				xx

	    in	[ SComment $ "Call " ++ show v ]
	     ++ (map SSlotPush boxedArgs)
	     ++ [ SAssign x t (XCall v unboxedArgs)
	        , SSlotPop (length boxedArgs) 
		, SBlank ]


	_ -> [s]
	
-}	



