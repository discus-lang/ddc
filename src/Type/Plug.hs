
module Type.Plug
	( plugClassIds
	, staticRsDataT
	, staticRsClosureT
	)

where

import Util
import Shared.Error

import Type.Exp
import Type.Util
import Type.Plate
import Type.Pretty

import Type.State
import Type.Class

import Debug.Trace

-----
stage	= "Type.Plug"


plugClassIds env xx
	= transZM (plugTable env) xx

-----
plugTable env
	= transTableId
	{ transV	= sinkVar
	, transT_leave	= plugT env}


plugT env t
 = case t of
	TClass k cid
	 | elem cid env	
	 -> 	return t

	 | otherwise
	 -> do	var	<- makeClassName cid
		Just c	<- lookupClass cid
	 	return	$ TVar (classKind c) var
		
	_ -> 	return t
	


-----
-- staticRsDataT
--	return the list of region classes which are non-generalisable because
--	they appear in non-function types.
--
staticRsDataT :: Type -> [ClassId]
staticRsDataT tt
 = case tt of
	TVar{}			-> []
	TClass k cid		
	 | k == KRegion		-> [cid]
	 | otherwise		-> []

 	TData v ts		-> catMap staticRsDataT ts
	TFun{}			-> []
	TFetters fs t		-> staticRsDataT t
	TForall vks t		-> staticRsDataT t
	
	TFree v t		-> staticRsDataT t
	
	TError k t		-> []
	
	-- for data containing function objects
	TEffect{}		-> []
	
	_ 	-> panic stage
		$ "staticRsDataT: " ++ show tt
		
-----
-- staticRsClosureT
--	Region cids that are free in the closure of the outer-most function
--	constructor(s) are being shared with the caller. These functions
--	did not allocate those regions, so they be can't generalised here.
--
staticRsClosureT
	:: Type -> [ClassId]

staticRsClosureT t
 = case t of
 	TFetters fs x	
	 -> let -- work out which cids appear on the outer-most function arrow
	 	outerCids	= outerFunClosureCids x

		-- collect up the closure bound to all these cids
		outerClo	= concat
				$ [flattenTSum clo 	| FLet (TClass KClosure cid) clo <- fs
							, elem cid outerCids ]

		-- see what regions are free in the closure
		staticRs	= concat
				$ [staticRsDataT t	
					| TFree v t	
					<- outerClo]

	   in	nub staticRs

	_ -> []
	
-----
-- outerFunClosureCids
--	Return the cids marking the closures of the outermost functions
--	in this type.
--
outerFunClosureCids tt
 = case tt of
 	TFun  t1 t2 eff (TClass KClosure c)	-> [c]
	TData v  ts				-> catMap outerFunClosureCids ts
	_					-> []







