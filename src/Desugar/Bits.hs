
module Desugar.Bits
	( isXVar
	, getAnnotX
	, getAnnotW
	, addLambdas )
where
import DDC.Desugar.Exp
import DDC.Main.Error
import DDC.Var

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

	

-- | Add some lambdas to the front of this expression
addLambdas 
	:: a	 			-- ^ source position to use on new nodes
	-> [Var]			-- ^ vars to bind with new lambdas
	-> Exp a 			-- ^ expressio to use as the body
	-> Exp a

addLambdas sp [] x	= x
addLambdas sp (v:vs) x	= XLambda sp v (addLambdas sp vs x)
