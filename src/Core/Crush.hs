-- Core.Crush
--	Crush together nested do blocks.
--	Crush together nester XTaus. 
module Core.Crush
(
	crushTree
)

where

import Core.Exp
import Core.Plate.Trans
import Core.Pretty

import Util
import Shared.Error

-----
stage	= "Core.Crush"

-----
crushTree :: Tree -> Tree
crushTree tree
	= transformSS crushSS tree
	
crushSS :: [Stmt] -> [Stmt]
crushSS	ss
	= catMap crushS ss

crushS :: Stmt	-> [Stmt]
crushS	s
 = case s of
	SBind mV x
	 -> let (ss, x')	= milk crushX x
	    in	ss ++ [SBind mV x']
    
	_			-> [s]


crushX xx
 = case xx of
 	XDo ss
	 -> let	is			= init ss
	 	Just (SBind Nothing x)	= takeLast ss
	    in	Just (is, x)

	XTau t1 (XTau t2 x)
	 | t1 == t2
	 -> Just ([], XTau t1 x)

	 | otherwise
	 -> panic stage
		$ "crushX: nested XTaus have different types.\n"
		% "   t1  = " %> t1		% "\n\n"
		% "   t2  = " %> t2		% "\n\n"
		% "   exp = " %> xx		% "\n\n"
	 
	XTau t x
	 -> liftM (\(ss, x') -> (ss, XTau t x'))
	 $ crushX x
	 
	XTet vts x
	 -> liftM (\(ss, x') -> (ss, XTet vts x'))
	 $ crushX x
	 	 
	_ 	-> Nothing
	



