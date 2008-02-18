
module Type.Util.StripFetters
	( stripFettersT 
	, stripMonoFLetsT )
where

import Util
import Shared.Error
import Type.Exp

-----
stage	= "Type.Util.StripFetters"


stripFettersT :: Type	-> (Type, [Fetter])
stripFettersT	tt
 = case tt of
	TForall vks t
	 -> let	(t', fs)	= stripFettersT t
	    in	( TForall vks t'
	    	, fs)

	TFetters fs t
	 -> let	(t', fs')	= stripFettersT t
	    in	( t'
	    	, fs ++ fs')
	
{-	TUnify k ts
	 -> let	(ts', fss)	= unzip $ map stripFettersT ts
	    in	( TUnify k ts'
	    	, concat fss)
-}
	TSum k ts
	 -> let	(ts', fss)	= unzip $ map stripFettersT ts
	    in	( TSum k ts'
	    	, concat fss)

	TMask k t1 t2
	 -> let	(t1', fs1)	= stripFettersT t1
	 	(t2', fs2)	= stripFettersT t2
	   in	( TMask k t1' t2'
	   	, fs1 ++ fs2 )

 	TVar{}	-> (tt, [])
	TBot{}	-> (tt, [])
	TTop{}	-> (tt, [])

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
	 -> (tt, [])

	TAccept t
	 -> let (t', fs)	= stripFettersT t
	    in	( t' 
	    	, fs)

	 
	TClass k cid	-> (tt, [])

	_	-> panic stage ("stripFettersT: no match for " % show tt)


-- | Strip all the monomorphic FLets from this type.
--	TODO: merge this code into stripFettersT
--
stripMonoFLetsT
	:: Type -> (Type, [Fetter])
	
stripMonoFLetsT tt
 = case tt of
 	TForall vks t	
	 -> let (t', fsMono)	= stripMonoFLetsT t
	    in	(TForall vks t', fsMono)
	 
	TFetters fs t
	 -> let	(fsMono, fsOther)	= partition isMonoFLet fs
		(t', fsMono2)		= stripMonoFLetsT t

	    in	(TFetters fsOther t', fsMono ++ fsMono2)
	    
	TSum k ts
	 -> let	(ts', fssMono)		= unzip $ map stripMonoFLetsT ts
	    in	(TSum k ts', concat fssMono)
	    
	TMask k t1 t2
	 -> let	(t1', fsMono1)		= stripMonoFLetsT t1
	 	(t2', fsMono2)		= stripMonoFLetsT t2
	    in	(TMask k t1' t2', fsMono1 ++ fsMono2)
	    
	TVar{}	-> (tt, [])
	TTop{}	-> (tt, [])
	TBot{}	-> (tt, [])
	
	TData v ts
	 -> let (ts', fssMono)		= unzip $ map stripMonoFLetsT ts
	    in	(TData v ts', concat fssMono)
	
	TFun t1 t2 eff clo
	 -> let	(t1',  fsMono1)		= stripMonoFLetsT t1
	 	(t2',  fsMono2)		= stripMonoFLetsT t2
		(eff', fsMonoE)		= stripMonoFLetsT eff
		(clo', fsMonoC)		= stripMonoFLetsT clo

	    in	( TFun t1' t2' eff' clo'
	    	, fsMono1 ++ fsMono2 ++ fsMonoE ++ fsMonoC)

	TEffect v ts 
	 -> let	(ts', fssMono)		= unzip $ map stripMonoFLetsT ts
	    in	(TEffect v ts', concat fssMono)
	    
	TFree v t
	 -> let	(t',  fsMono)		= stripMonoFLetsT t
	    in	(TFree v t', fsMono)
	    
	TTag{}	-> (tt, [])
	
	TClass{} -> (tt, [])

isMonoFLet ff
 = case ff of
	FLet TClass{} _	-> True
	_		-> False

