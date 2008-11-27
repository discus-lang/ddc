
module Bits
	( argTypesT
	, resultTypeT
	, isFun
	, makeCall
	, none
	, varV
	, tInt, tFun
	, ePure
	, cEmpty
	, xVar
	, initEnv)
	
where

import Source.Util
import Source.Exp

import Type.Util
import Type.Exp

import Shared.VarPrim
import Shared.Var		(NameSpace(..))
import Shared.Base
import qualified Shared.Var	as Var

-- | return the types of the arguments of this function
argTypesT :: Type -> [Type]
argTypesT tt
 = case tt of
 	TFun t1 t2 _ _	-> t1 : argTypesT t2
	_		-> []

-- | return the result type of this function,
--	or id if it isn't
resultTypeT :: Type -> Type
resultTypeT tt
 = case tt of
 	TFun t1 t2 _ _	-> resultTypeT t2
	_		-> tt

-- | test whether a type is a function
isFun :: Type -> Bool
isFun tt
 = case tt of
 	TFun{}		-> True
	_		-> False

-- | make a function call
makeCall 	= unflattenApps none

-- we don't use source positions in the Source.Exp type
none	= error "no source position"

varV s		= (Var.new s) { Var.nameSpace = NameValue }
tInt		= TData KValue (primTInt (UnboxedBits 32)) []
tFun t1 t2	= TFun t1 t2 ePure cEmpty
ePure		= TBot KEffect
cEmpty		= TBot KClosure

initEnv		= [(tFun tInt (tFun tInt tInt), varV "+")]

xVar s		= XVar none (varV s)
