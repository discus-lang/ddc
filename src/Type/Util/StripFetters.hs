{-# OPTIONS -fwarn-incomplete-patterns #-}

module Type.Util.StripFetters
	( stripMonoFLetsT 
	, stripFLetsT)
where

import Util
import Shared.Error
import Type.Pretty
import Type.Exp

stage = "Type.Util.StripFetters"

stripFLetsT :: Type	-> (Type, [Fetter])
stripFLetsT	tt
 = case tt of
	TNil	-> (TNil, [])

	TForall b k t
	 -> let	(t', fs)	= stripFLetsT t
	    in	( TForall b k t'
	    	, fs)

	TFetters t fs
	 -> let	(t', fs')	= stripFLetsT t
		(fsLet, fsOther)	
				= partition isFLet fs
	    in	( TFetters t' fsOther
	    	, fsLet ++ fs')
	
	TSum k ts
	 -> let	(ts', fss)	= unzip $ map stripFLetsT ts
	    in	( TSum k ts'
	    	, concat fss)

	TMask k t1 t2
	 -> let	(t1', fs1)	= stripFLetsT t1
	 	(t2', fs2)	= stripFLetsT t2
	   in	( TMask k t1' t2'
	   	, fs1 ++ fs2 )

	TApp t1 t2
	 -> let	(t1', fs1)	= stripFLetsT t1
	 	(t2', fs2)	= stripFLetsT t2
	    in	( TApp t1' t2'
	    	, fs1 ++ fs2)
		
	TCon tc	-> (tt, [])
	

 	TVar{}	-> (tt, [])
	TBot{}	-> (tt, [])
	TTop{}	-> (tt, [])
	TWild{}	-> (tt, [])
	

	-- data
	TFun t1 t2 eff clo
	 -> let	(t1', f1)	= stripFLetsT t1 
		(t2', f2)	= stripFLetsT t2
		(eff', fsEff)	= stripFLetsT eff
		(clo', fsClo)	= stripFLetsT clo
	    in
	    	( TFun t1' t2' eff' clo'
		, f1 ++ f2 ++ fsEff ++ fsClo)
		
	TData k v ts
	 -> let	(ts', fss)	= unzip $ map stripFLetsT ts
	    in
	    	( TData k v ts'
		, concat fss)
		
	-- effect
	TEffect v ts
	 -> let	(ts', fss)	= unzip $ map stripFLetsT ts
	    in	( TEffect v ts'
	    	, concat fss)

	-- closure
	TFree v t
	 -> let	(t', fs)	= stripFLetsT t
	    in	( TFree v t'
	    	, fs)

	TDanger t1 t2
	 -> let (t1', fs1)	= stripFLetsT t1
	  	(t2', fs2)	= stripFLetsT t2
	    in	(TDanger t1' t2'
	        , fs1 ++ fs2)

	TTag v		-> (tt, [])
	TClass k cid	-> (tt, [])

	_ -> panic stage
		$ "stripFLetsT: no match for " % tt % "\n"

isFLet ff
 = case ff of
	FLet _ _	-> True
	_		-> False


-- | Strip all the monomorphic FLets from this type.
--	TODO: merge this code into stripFLets
--
stripMonoFLetsT
	:: Type -> (Type, [Fetter])
	
stripMonoFLetsT tt
 = case tt of
	TNil	-> (tt, [])

 	TForall b k t	
	 -> let (t', fsMono)	= stripMonoFLetsT t
	    in	(TForall b k t', fsMono)
	 
	TFetters t fs
	 -> let	(fsMono, fsOther)	= partition isMonoFLet fs
		(t', fsMono2)		= stripMonoFLetsT t

	    in	(TFetters t' fsOther, fsMono ++ fsMono2)
	    
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
	
	TApp t1 t2
	 -> let	(t1', fsMono1)		= stripMonoFLetsT t1
		(t2', fsMono2)		= stripMonoFLetsT t2
	    in	( TApp t1' t2'
		, fsMono1 ++ fsMono2)
	
	TData k v ts
	 -> let (ts', fssMono)		= unzip $ map stripMonoFLetsT ts
	    in	(TData k v ts', concat fssMono)
	
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
	    
	TDanger t1 t2 
	 -> let (t1', fsMono1)		= stripMonoFLetsT t1
	 	(t2', fsMono2)		= stripMonoFLetsT t2
	    in	( TDanger t1' t2'
	    	, fsMono1 ++ fsMono2)
	    
	TTag{}	-> (tt, [])
	
	TClass{} -> (tt, [])
	TWild{}	-> (tt, [])

	TError{} -> (tt, [])

	_ -> panic stage
		$ "stripMonoFLetsT: no match for " % tt 

isMonoFLet ff
 = case ff of
	FLet TClass{} _	-> True
	_		-> False

