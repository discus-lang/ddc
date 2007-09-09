-- Core.Crush
--	Crush together nested do blocks.
--	Crush together nester XTaus. 
module Core.Crush
(
	crushTree
)

where

import Util

import Core.Exp
import Core.Plate.Trans

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

{-
crushX :: Exp -> (Bool, [Stmt], Exp)
crushX xx
 = case xx of
	XDo ss	
	 -> let is			= init ss 
	    	Just (SBind Nothing x)	= takeLast ss
  	    in	(True, is, x)

	XTau t1 x@(XTau t2 _)
	 |  t1 == t2
	 -> let (_,    ss, x')	= crushX x
	    in	(True, ss, x')
	    
	XTau t x
	 -> let (ss, x')	= crushX x
	    in  (ss, XTau t x')
	    
	XTet v t x
	 -> let (ss, x')	= crushX x
	    in	(ss, XTet v t x')
	    
	_	-> ([], xx)
-}	

	

crushX xx
 = case xx of
 	XDo ss
	 -> let	is			= init ss
	 	Just (SBind Nothing x)	= takeLast ss
	    in	Just (is, x)

	XTau t1 (XTau t2 x)
	 | t1 == t2
	 -> Just ([], XTau t1 x)
	 
	XTau t x
	 -> liftM (\(ss, x') -> (ss, XTau t x'))
	 $ crushX x
	 
	XTet vts x
	 -> liftM (\(ss, x') -> (ss, XTet vts x'))
	 $ crushX x
	 	 
	_ 	-> Nothing
	



