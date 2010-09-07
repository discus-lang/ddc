{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Sea.Compounds
	(takeXVar)
where
import DDC.Sea.Exp


-- | Take the var from an XVar or XVarCaf
takeXVar :: Exp a -> Maybe Var
takeXVar xx
 = case xx of
	XVar     v _	-> Just v
	XVarCAF  v _ 	-> Just v
	XSlot    v _ _	-> Just v
	XSlotCAF v _ 	-> Just v 
	_		-> Nothing