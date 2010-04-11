-- |	Crush together nested do blocks.
--	Crush together nester XTaus. 
module Core.Crush
	( crushTree
	, crushGlob)
where
import Core.Exp
import Core.Glob
import Core.Plate.Trans
import Util

crushTree :: Tree -> Tree
crushTree tree
	= transformSS crushSS tree

crushGlob :: Glob -> Glob
crushGlob glob
	= mapBindsOfGlob (transformSS crushSS) glob
	
crushSS :: [Stmt] -> [Stmt]
crushSS	ss
	= catMap crushS ss

crushS :: Stmt	-> [Stmt]
crushS	s
 = case s of
	SBind mV x
	 -> let (ss, x')	= milk crushX x
	    in	ss ++ [SBind mV x']
    

crushX xx
 = case xx of
 	XDo ss
	 -> let	is			= init ss
	 	Just (SBind Nothing x)	= takeLast ss
	    in	Just (is, x)

	-- Crush out nested XTaus, 
	--	Although it would be nice, we can't check that t1 == t2 here because we don't have
	--	a map of let-bound types. It doesn't really matter though, because if the result is
	--	wrong it will get picked up by the checker in a subsequent pass.
	XTau t1 (XTau t2 x)
	 -> Just ([], XTau t1 x)

	XTau t x
	 -> liftM (\(ss, x') -> (ss, XTau t x'))
	 $ crushX x
	 
	_ 	-> Nothing
	



