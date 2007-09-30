
module Type.Plug
	( plugClassIds
	, staticRsDataT
--	staticRsClosureT
	)

where

import Util
import Shared.Error

import Type.Exp
import Type.Util
import Type.Plate

import Type.State
import Type.Class

-----
stage	= "Type.Squid.Plug"


plugClassIds :: [ClassId] -> Type -> SquidM Type
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
staticRsDataT :: Type -> SquidM [ClassId]
staticRsDataT	  t
 = case t of
	TVar{}			-> return []
	TClass k cid		
	 -> do	if k == KRegion 
		 then	return [cid]
		 else	return []
	 
 	TData v ts		-> liftM concat $ mapM staticRsDataT ts
	TFun{}			-> return []
	TFetters fs t		-> staticRsDataT t
	TForall vks t		-> staticRsDataT t
	TNode t1 t2		-> staticRsDataT t2
	TAccept	t		-> staticRsDataT t
	TUnify k ts		-> liftM concat $ mapM staticRsDataT ts
	
	_ -> panic stage
		$ "staticRsDataT: " ++ show t
		
-----
-- staticRsClosureT
--	Region cids that are free in the closure of the outer-most function
--	constructor(s) are being shared with the caller. These functions
--	did not allocate those regions, so they be can't generalised here.
--
staticRsClosureT
	:: Type -> SquidM [ClassId]


staticRsClosureT t
 = case t of
{-
 	TFetters fs x	
	 -> do	let outerCids	= outerFunClosureCids x
		let outerClo	= concat 
				$ [cs 	| FClosure (CClass cid) (CSum cs) <- fs
					, elem cid outerCids ]

		let staticRs	= concat
				$ [staticRsDataT t	
					| CFreeT v t	
					<- outerClo]

		traceM	$ "*   staticCidsClosureT " % t 	% "\n"
			% "    outerCids  = " % outerCids	% "\n"
			% "    outerClo   = " % outerClo	% "\n"
			% "    staticRs   = " % staticRs	% "\n"
			% "\n"

		return	$ nub staticRs
-}

	_ -> 	return	[]
	
-----
-- outerFunClosureCids
--	Return the cids marking the closures of the outermost functions
--	in this type.
--
{-
outerFunClosureCids t
 = case t of
 	TFun t1 t2 eff (CClass c)	-> [c]
	TCon v  ts			-> catMap outerFunClosureCids ts
	TClosure (CClass c)		-> [c]
	_				-> []
-}	






