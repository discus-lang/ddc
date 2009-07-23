-- | Utils for generating type expressions.
module Churn.Type
where

import Type.Exp
import Shared.VarPrim
import Shared.Base

tInt		= TData KValue (primTInt  Boxed) []
tBool		= TData KValue (primTBool Boxed) []
tFun t1 t2	= TFun t1 t2 ePure cEmpty
ePure		= TBot KEffect
cEmpty		= TBot KClosure


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

