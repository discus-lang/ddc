
module Core.Pack
	( eraseContextsT
	, inlineTWheresT
	, flattenT )

where

import Util
import Core.Exp
import Core.Plate.Trans

import Shared.Error

import qualified Data.Map	as Map
import Data.Map			(Map)

-----
stage	= "Core.Pack"



eraseContextsT :: Type -> Type
eraseContextsT tt
 = transformT eraseContextsT' tt

eraseContextsT' tt
 = case tt of
 	TContext c t	-> t
	_		-> tt
	
flattenT :: Type -> Type
flattenT tt
	= inlineTWheresT Map.empty tt

-----
-- inlineTWheresT
--	Inline all TLet expressions in this type.
--	
inlineTWheresT :: Map Var Type -> Type 	-> Type
inlineTWheresT sub tt
 = case tt of
 	TNil			-> tt
	
	TForall v k t
	 -> let	t'	= inlineTWheresT sub t
	    in	TForall v k t'
	    
	TWhere t1 vts		
	 -> inlineTWheresT (Map.union (Map.fromList vts) sub) t1

	TContext l t
	 -> let t'	= inlineTWheresT sub t
	    in	TContext l t'

	TSum k ts
	 -> let	ts'	= map (inlineTWheresT sub) ts
	    in	TSum k ts'

	TMask k t1 t2
	 -> let	t1'	= inlineTWheresT sub t1
	   	t2'	= inlineTWheresT sub t2
	    in	TMask k t1 t2
	    
	TVar k v	
	 -> case Map.lookup v sub of
	 	Just t	-> t
		_	-> tt
		
    
	-- data
	TFunEC t1 t2 eff clo
	 -> let	t1'	= inlineTWheresT sub t1
	 	t2'	= inlineTWheresT sub t2
		eff'	= inlineTWheresT sub eff
		clo'	= inlineTWheresT sub clo
	    in	TFunEC t1' t2' eff' clo'

	TData v ts
	 -> let	ts'	= map (inlineTWheresT sub) ts
	    in	TData v ts'


	-- region
	
	
	-- effect
	TEffect  v ts
	 -> let	ts'	= map (inlineTWheresT sub) ts
	    in	TEffect v ts'

 	TPure		-> tt
	 	
	-- closure
	TFree v t
	 -> let t'	= inlineTWheresT sub t
	    in	TFree v t'

	TTag v		-> tt

	TEmpty		-> tt

	TKind k		-> tt
	    
	_ -> panic stage
		$ "inlineTWheresT: no match for " % show tt



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
