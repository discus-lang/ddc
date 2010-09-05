{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Bits and pieces for working with desugared expresions.
module DDC.Desugar.Bits
	( isXVar
	, takeAnnotX
	, getAnnotW
	, addLambdas )
where
import DDC.Desugar.Exp
import DDC.Var


-- | Check if some exp is an @XVar@
isXVar :: Exp a -> Bool
isXVar xx
 = case xx of
 	XVar{}	-> True
	_	-> False


-- | Get the annotation from this expression.
takeAnnotX :: Exp a -> Maybe a
takeAnnotX xx
 = case xx of
 	XNil				-> Nothing
	XVoid 		n		-> Just n
	XLit		n _		-> Just n
	XVar 		n _		-> Just n
	XProj 		n _ _		-> Just n
	XLambda 	n _ _		-> Just n
	XApp 		n _ _		-> Just n
	XMatch 		n _ _		-> Just n
	XDo		n _		-> Just n
	XIfThenElse	n _ _ _		-> Just n
	XLambdaTEC	n _ _ _ _ _	-> Just n
	XProjTagged	n _ _ _ _	-> Just n
	XProjTaggedT	n _ _ _ 	-> Just n
	XProjT		n _ _ 		-> Just n
	XVarInst	n _		-> Just n
	
	
-- | Get the annotation from this pattern.
getAnnotW :: Pat a -> a
getAnnotW ww
 = case ww of
	WConLabel	n _ _		-> n
	WLit		n _		-> n
	WVar		n _		-> n
	WAt		n _ _		-> n
	WConLabelP	n _ _		-> n

	
-- | Add some lambdas to the front of an expression.
addLambdas 
	:: a	 	-- ^ Annotation to use on new nodes.
	-> [Var]	-- ^ Vars to bind.
	-> Exp a 	-- ^ Expression to use as the body.
	-> Exp a

addLambdas _ [] x	= x
addLambdas sp (v:vs) x	= XLambda sp v (addLambdas sp vs x)
