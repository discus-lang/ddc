
module Core.Bits
where

import Util
import Core.Exp


isXApp x
	= or
	[ x =@= XAPP{}
	, x =@= XApp{} ]


isXLambda x	= (x =@= XLam{})
isXLAMBDA x	= (x =@= XLAM{})
isXTau x	= (x =@= XTau{})

isTForall x	= x =@= TForall{}



crushSumT :: Type -> [Type]
crushSumT tt
 = case tt of
 	TPure		-> []
	TEmpty		-> []
	TSum k ts	-> catMap crushSumT ts
	_		-> [tt]
	

makeSumT :: Kind -> [Type] -> Type
makeSumT k ts
 = case catMap crushSumT ts of
 	[]	-> case k of
			KEffect		-> TPure
			KClosure	-> TEmpty
	ts'	-> TSum k ts'
	
 


		
