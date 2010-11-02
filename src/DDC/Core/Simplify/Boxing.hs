{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Core.Simplify.Boxing
	(simplifyUnboxBoxX)
where
import DDC.Core.Exp
import Control.Monad.State.Strict

-- | Zap out unbox/box and unbox/force/box primops.
simplifyUnboxBoxX :: Exp -> State Int Exp
simplifyUnboxBoxX xx
 = case xx of 

	-- unbox/box
 	XPrim MUnbox [r1, XPrim MBox [r2, x]]
		| r1 == r2	
		-> do	modify (+ 1)
			return	x
		
	-- unbox/force/box
	XPrim MUnbox [r1, XPrim MForce [XPrim MBox [r2, x]]]
		| r1 == r2
		-> do	modify (+ 1)
			return x
		
	_	-> return xx
