
module Desugar.Bits
	( getAnnotX
	, getAnnotW
	, isXVar)
	
where

import Shared.Error
import Desugar.Exp

stage	= "Desugar.Exp"

isXVar xx
 = case xx of
 	XVar{}	-> True
	_	-> False


-- | get the annotation from this expression
getAnnotX :: Exp a -> a
getAnnotX xx
 = case xx of
 	XNil				-> panic stage $ "getAnnotX: not annot in XNil"
	XVoid 		n		-> n
	XLit		n l		-> n
	XVar 		n v		-> n
	XProj 		n x j		-> n
	XLambda 	n v x		-> n
	XApp 		n x1 x2		-> n
	XMatch 		n x aa		-> n
	XDo		n ss		-> n
	XIfThenElse	n e1 e2 e3	-> n
	XLambdaTEC	n v x t eff clo	-> n
	XProjTagged	n vI vC x j	-> n
	XVarInst	n v		-> n
	
	
-- | get the annotation from this pattern
getAnnotW :: Pat a -> a
getAnnotW ww
 = case ww of
	WConLabel	n _ _		-> n
	WLit		n _		-> n
	WVar		n _		-> n
	WAt		n _ _		-> n
	WConLabelP	n _ _		-> n

	
