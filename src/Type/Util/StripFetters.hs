{-# OPTIONS -fwarn-incomplete-patterns #-}

module Type.Util.StripFetters
	( stripMonoFWheresT 
	, stripFWheresT)
where

import Util
import Shared.Error
import Type.Pretty
import Type.Exp

stage = "Type.Util.StripFetters"

stripFWheresT :: Type	-> (Type, [Fetter])
stripFWheresT	tt
 = case tt of
	TNil	-> (TNil, [])

	TForall b k t
	 -> let	(t', fs)	= stripFWheresT t
	    in	( TForall b k t'
	    	, fs)

	TFetters t fs
	 -> let	(t', fs')	= stripFWheresT t
		(fsLet, fsOther)	
				= partition isFWhere fs
	    in	( TFetters t' fsOther
	    	, fsLet ++ fs')
	
	TSum k ts
	 -> let	(ts', fss)	= unzip $ map stripFWheresT ts
	    in	( TSum k ts'
	    	, concat fss)

	TApp t1 t2
	 -> let	(t1', fs1)	= stripFWheresT t1
	 	(t2', fs2)	= stripFWheresT t2
	    in	( TApp t1' t2'
	    	, fs1 ++ fs2)
		
	TCon tc	-> (tt, [])

 	TVar{}	-> (tt, [])
	TBot{}	-> (tt, [])
	TTop{}	-> (tt, [])
	TWild{}	-> (tt, [])

	-- data
	TFun t1 t2 eff clo
	 -> let	(t1', f1)	= stripFWheresT t1 
		(t2', f2)	= stripFWheresT t2
		(eff', fsEff)	= stripFWheresT eff
		(clo', fsClo)	= stripFWheresT clo
	    in
	    	( TFun t1' t2' eff' clo'
		, f1 ++ f2 ++ fsEff ++ fsClo)
		
	TData k v ts
	 -> let	(ts', fss)	= unzip $ map stripFWheresT ts
	    in
	    	( TData k v ts'
		, concat fss)
		
	-- effect
	TEffect v ts
	 -> let	(ts', fss)	= unzip $ map stripFWheresT ts
	    in	( TEffect v ts'
	    	, concat fss)

	-- closure
	TFree v t
	 -> let	(t', fs)	= stripFWheresT t
	    in	( TFree v t'
	    	, fs)

	TDanger t1 t2
	 -> let (t1', fs1)	= stripFWheresT t1
	  	(t2', fs2)	= stripFWheresT t2
	    in	(TDanger t1' t2'
	        , fs1 ++ fs2)

	TClass k cid	-> (tt, [])

	_ -> panic stage
		$ "stripFWheresT: no match for " % tt % "\n"

isFWhere ff
 = case ff of
	FWhere _ _	-> True
	_		-> False


-- | Strip all the monomorphic FWheres from this type.
--	TODO: merge this code into stripFWheres
--
stripMonoFWheresT
	:: Type -> (Type, [Fetter])
	
stripMonoFWheresT tt
 = case tt of
	TNil	-> (tt, [])

 	TForall b k t	
	 -> let (t', fsMono)	= stripMonoFWheresT t
	    in	(TForall b k t', fsMono)
	 
	TFetters t fs
	 -> let	(fsMono, fsOther)	= partition isMonoFWhere fs
		(t', fsMono2)		= stripMonoFWheresT t

	    in	(TFetters t' fsOther, fsMono ++ fsMono2)
	    
	TSum k ts
	 -> let	(ts', fssMono)		= unzip $ map stripMonoFWheresT ts
	    in	(TSum k ts', concat fssMono)
	    
	TVar{}	-> (tt, [])
	TTop{}	-> (tt, [])
	TBot{}	-> (tt, [])
	
	TApp t1 t2
	 -> let	(t1', fsMono1)		= stripMonoFWheresT t1
		(t2', fsMono2)		= stripMonoFWheresT t2
	    in	( TApp t1' t2'
		, fsMono1 ++ fsMono2)
	
	TData k v ts
	 -> let (ts', fssMono)		= unzip $ map stripMonoFWheresT ts
	    in	(TData k v ts', concat fssMono)
	
	TFun t1 t2 eff clo
	 -> let	(t1',  fsMono1)		= stripMonoFWheresT t1
	 	(t2',  fsMono2)		= stripMonoFWheresT t2
		(eff', fsMonoE)		= stripMonoFWheresT eff
		(clo', fsMonoC)		= stripMonoFWheresT clo

	    in	( TFun t1' t2' eff' clo'
	    	, fsMono1 ++ fsMono2 ++ fsMonoE ++ fsMonoC)

	TEffect v ts 
	 -> let	(ts', fssMono)		= unzip $ map stripMonoFWheresT ts
	    in	(TEffect v ts', concat fssMono)
	    
	TFree v t
	 -> let	(t',  fsMono)		= stripMonoFWheresT t
	    in	(TFree v t', fsMono)
	    
	TDanger t1 t2 
	 -> let (t1', fsMono1)		= stripMonoFWheresT t1
	 	(t2', fsMono2)		= stripMonoFWheresT t2
	    in	( TDanger t1' t2'
	    	, fsMono1 ++ fsMono2)
	    
	TClass{} -> (tt, [])
	TWild{}	-> (tt, [])

	TError{} -> (tt, [])

	_ -> panic stage
		$ "stripMonoFWheresT: no match for " % tt 

isMonoFWhere ff
 = case ff of
	FWhere TClass{} _	-> True
	_			-> False

