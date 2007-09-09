
module Type.Util.StripFetters
	( stripFettersT )
where

import Util
import Shared.Error
import Type.Exp

-----
stage	= "Type.Util.StripFetters"


stripFettersT :: Type	-> (Type, [Fetter])
stripFettersT	t
 = case t of
	TForall vks t
	 -> let	(t', fs)	= stripFettersT t
	    in	( TForall vks t'
	    	, fs)

	TFetters fs t
	 -> let	(t', fs')	= stripFettersT t
	    in	( t'
	    	, fs ++ fs')
	
	TUnify k ts
	 -> let	(ts', fss)	= unzip $ map stripFettersT ts
	    in	( TUnify k ts'
	    	, concat fss)

	TSum k ts
	 -> let	(ts', fss)	= unzip $ map stripFettersT ts
	    in	( TSum k ts'
	    	, concat fss)

	TMask k t1 t2
	 -> let	(t1', fs1)	= stripFettersT t1
	 	(t2', fs2)	= stripFettersT t2
	   in	( TMask k t1' t2'
	   	, fs1 ++ fs2 )

 	TVar{}	
	 -> (t, [])

	-- data
	TFun t1 t2 eff clo
	 -> let	(t1', f1)	= stripFettersT t1 
		(t2', f2)	= stripFettersT t2
		(eff', fsEff)	= stripFettersT eff
		(clo', fsClo)	= stripFettersT clo
	    in
	    	( TFun t1' t2' eff' clo'
		, f1 ++ f2 ++ fsEff ++ fsClo)
		
	TData v ts
	 -> let	(ts', fss)	= unzip $ map stripFettersT ts
	    in
	    	( TData v ts'
		, concat fss)
		
	-- effect
	TEffect v ts
	 -> let	(ts', fss)	= unzip $ map stripFettersT ts
	    in	( TEffect v ts'
	    	, concat fss)

	-- closure
	TFree v t
	 -> let	(t', fs)	= stripFettersT t
	    in	( TFree v t'
	    	, fs)

	TTag v
	 -> (t, [])

	TAccept t
	 -> let (t', fs)	= stripFettersT t
	    in	( t' 
	    	, fs)

	 
	TClass k cid	-> (t, [])

	_	-> panic stage ("stripFettersT: no match for " % show t)

	 
