
module Desugar.ToCore.Util
(
--	addFunLambdas,
	pushLambdaXDo,
	doMe
)

where

import Shared.Var (Var)
import qualified Core.Exp 	as C

import Desugar.ToCore.Base

-----
stage	= "Desugar.ToCore.Util"





-----------------------
-- addFunLambdas
--	Takes an exp and a list of variables.
--	Binds all the variables with fresh lambdas around the exp.
--
--	Also looks up the types for the vars as it goes, so the lambdas
--	will contain the types for the bound variables.
--
--	Makes sure that the body of the inner most lambda is an XDo
--
{-
addFunLambdas :: C.Exp	-> [Var]	-> CoreM C.Exp
addFunLambdas    e vs
 = case vs of
 	[]	-> return e
	(v:vs)	
	 -> do
		t	<- getNode v
		eR	<- addFunLambdas e vs
		return	$  C.XLam v t eR
-}


-----------------------
-- pushLambdaXDo
--	Makes sure that the body of an inner most lambda is an XDo
--
pushLambdaXDo :: C.Exp -> C.Exp
pushLambdaXDo	 xx
 = case xx of
	C.XLAM{}	-> pushLambdaXDo' xx
 	C.XLam{}	-> pushLambdaXDo' xx
	_		-> xx
	
pushLambdaXDo'	xx
 = case xx of
	C.XLAM v k e
	 -> C.XLAM v k $ pushLambdaXDo' e

 	C.XLam v t e eff clo	
	 -> C.XLam v t (pushLambdaXDo' e) eff clo
	
	C.XDo [C.SBind _ x]
	 -> pushLambdaXDo' x
	 		
	C.XDo{}
	 -> xx

	C.XAnnot _ (x@C.XDo  {})	-> pushLambdaXDo' x
	C.XAnnot _ (x@C.XLam {})	-> pushLambdaXDo' x

--	C.XAnnot [C.NType t] x 	
--	 -> C.XDo [C.SBind Nothing t x]

	_ -> C.XDo [C.SBind Nothing xx]
 

--	_ -> panic stage
--		$ "pushLambdaXDo: no match for " % show xx % "\n"
	
doMe :: 	C.Exp -> C.Exp
doMe		x
 = case x of
	C.XLam v t x' eff clo	-> C.XLam v t (doMe x') eff clo
	C.XLAM v k x'		-> C.XLAM v k (doMe x')
 	C.XDo{}			-> x
	_			-> C.XDo [C.SBind Nothing x]


stripAnnot xx
 = case xx of
 	C.XAnnot _ x	-> x
	_		-> xx
