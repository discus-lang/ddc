
module Core.Pack
	(eraseContextsT)

where

import Util
import Core.Exp
import Core.Plate.Trans


eraseContextsT :: Type -> Type
eraseContextsT tt
 = transformT eraseContextsT' tt

eraseContextsT' tt
 = case tt of
 	TContext c t	-> t
	_		-> tt
	



{-
packType ::	Type	-> Type
packType	t
 	= liftFettersT
 	$ t


eraseFettersT :: Type	-> Type
eraseFettersT	 t
 	= fst $ stripFsT t

liftFettersT :: Type	-> Type
liftFettersT	t
 = case t of
 	TForall v k t
	 -> TForall v k (liftFettersT t)
	_ 
	 -> let	(t', fs)	= stripFsT t
   	    in	addFettersT fs t'


stripFsT ::	Type	-> (Type, [Class])
stripFsT	tt
 = case tt of
	TNil{}		-> (tt, [])
 	TVar{}		-> (tt, [])

	TForall v k t
	 -> let	(t', fs)	= stripFsT t
	    in	( TForall v k t'
	    	, fs)

	TFun t1 t2
	 -> let	(t1', f1)	= stripFsT t1
	 	(t2', f2)	= stripFsT t2
	    in	( TFun t1' t2'
	        , f1 ++ f2)

	TFunEE t1 t2 eff env
	 -> let	(t1', f1)	= stripFsT t1 
		(t2', f2)	= stripFsT t2
	    in
	    	( TFunEE t1' t2' eff env
		, f1 ++ f2)
		
	TCon v ts
	 -> let	(ts', fss)	= unzip 
	 			$ map stripFsT ts
	    in
	    	( TCon v ts'
		, concat fss)

	TClass mV c t	
	 -> let	(t', fs)	= stripFsT t
	    in	( t'
	    	, c : fs)
		
	TKind{}		-> (tt, [])
	TRegion{}	-> (tt, [])
	TEffect{}	-> (tt, [])	 
	TClosure{}	-> (tt, [])


addFettersT ::	[Class] -> Type	-> Type
addFettersT	[]     t	= t
addFettersT	(c:fs) t	= TClass Nothing c (addFettersT fs t)
-}
