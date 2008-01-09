
module Desugar.Bits
	( getAnnotX
	, isXVar)
	
where

import Shared.Error
import Desugar.Exp

stage	= "Desugar.Exp"

isXVar xx
 = case xx of
 	XVar{}	-> True
	_	-> False

getAnnotX :: Exp a -> a
getAnnotX xx
 = case xx of
 	XNil				-> panic stage $ "getAnnotX: not annot in XNil"
	XVoid 		n		-> n
	XConst 		n c		-> n
	XVar 		n v		-> n
	XProj 		n x j		-> n
	XLambda 	n v x		-> n
	XApp 		n x1 x2		-> n
	XMatch 		n x aa		-> n
	XDo		n ss		-> n
	XIfThenElse	n e1 e2 e3	-> n
	XLambdaTEC	n v x t eff clo	-> n
	XProjTagged	n v x j		-> n
	XVarInst	n v		-> n
	
	
	
	
