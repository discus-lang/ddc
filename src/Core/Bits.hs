
module Core.Bits
	( isXApp
	, isXLambda
	, isXLAMBDA
	, isXTau
	, isTForall
	
	, crushSumT
	, makeSumT
	
	, kindOfSpace )
where

import Util
import Core.Exp
import Shared.Var
import Shared.Error

-----
stage	= "Core.Bits"

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
 	TBot k 		-> []
	TSum k ts	-> catMap crushSumT ts
	_		-> [tt]
	

makeSumT :: Kind -> [Type] -> Type
makeSumT k ts
 = case catMap crushSumT ts of
 	[]	-> TBot k
	ts'	-> TSum k ts'
	
 

		
-- | Get the kind associated with a namespace.
kindOfSpace :: NameSpace -> Kind
kindOfSpace space
 = case space of
 	NameType	-> KData
	NameRegion	-> KRegion
	NameEffect	-> KEffect
	NameClosure	-> KClosure
	NameClass	-> KClass
	_		-> panic stage
			$  "kindOfSpace: no match for " % show space
