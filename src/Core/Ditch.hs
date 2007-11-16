{-# OPTIONS -fwarn-incomplete-patterns #-}

-- Ditch extraneous type information in preparation for conversion to Sea.
--
module Core.Ditch 
	(ditchTree)
where

import Util
import Core.Exp
import Core.Plate.Trans
import Core.Pretty
import Shared.Error

-----
stage	= "Core.Ditch"

ditchTree ::	Tree -> Tree
ditchTree	= transformX ditchX


ditchX :: Exp	-> Exp
ditchX	xx 
 = case xx of
--	XAnnot	n x		-> ditchX x
	XLAM	v t x		-> ditchX x
 	XAPP	x t		-> ditchX x
	XTet	vts x		-> ditchX x
	XTau	t x		-> XTau t (ditchX x)
	XLam	v t x eff clo	-> XLam v t (ditchX x) TSync TOpen
	XApp	x1 x2 eff	-> XApp (ditchX x1) (ditchX x2) TSync
	XDo	ss		-> XDo  (map ditchS ss)
	XMatch	aa eff		-> XMatch (map ditchA aa) TSync
	XConst{}		-> xx
	XVar{}			-> xx
 	XLocal v vs x		-> ditchX x
	XPrim  v xx eff		-> XPrim v xx TSync
	
	_			-> xx
	
ditchS :: Stmt	-> Stmt
ditchS	(SBind v x)		= SBind v (ditchX x)

ditchA :: Alt -> Alt
ditchA	(AAlt gs x)		= AAlt (map ditchG gs) (ditchX x)

ditchG :: Guard -> Guard
ditchG	(GExp p x)		= GExp p (ditchX x)


