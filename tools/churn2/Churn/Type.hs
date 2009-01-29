-- | Utils for generating type expressions.
module Churn.Type
where

import Type.Exp
import Shared.VarPrim
import Shared.Base

tInt		= TData KValue (primTInt (UnboxedBits 32)) []
tFun t1 t2	= TFun t1 t2 ePure cEmpty
ePure		= TBot KEffect
cEmpty		= TBot KClosure
