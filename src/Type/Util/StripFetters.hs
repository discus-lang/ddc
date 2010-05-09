{-# OPTIONS -fwarn-incomplete-patterns #-}

module Type.Util.StripFetters
	( stripFWheresT_all
	, stripFWheresT_mono )
where
import Type.Exp
import Util
import DDC.Main.Error
import Type.Pretty		()

-----
stage = "Type.Util.StripFetters"

-- | Strip all fetters from this type
stripFWheresT_all  :: Type -> (Type, [Fetter])
stripFWheresT_all  = stripFWheresT False


-- | Strip just the monomorphic FWhere fetters, 
--	leaving the others still attached.
stripFWheresT_mono :: Type -> (Type, [Fetter])
stripFWheresT_mono = stripFWheresT True


-- | Worker function for above
stripFWheresT 
	:: Bool 		-- ^ whether to only stip monomorphic FWhere fetters
	-> Type			-- ^ the type to strip
	-> (Type, [Fetter])

stripFWheresT justMono	tt
 = case tt of
	TNil	 -> (tt, [])

	TError{} -> (tt, [])

	TForall b k t
	 -> let	(t', fs)	= stripFWheresT justMono t
	    in	( TForall b k t'
	    	, fs)

	TFetters t fs
	 -- Just take the monomorphic FWheres
	 | justMono
	 -> let	(t', fsMono)		= stripFWheresT justMono t
		(fsMono2, fsOther)	= partition isMonoFWhere fs

	    in	( TFetters t' fsOther
	        , fsMono ++ fsMono2)

         -- Take all of thw FWheres
         | otherwise
	 -> let	(t', fs')		= stripFWheresT justMono t
		(fsWhere, fsOther)	= partition isFWhere fs

	    in	( TFetters t' fsOther
	    	, fsWhere ++ fs')
	
	TSum k ts
	 -> let	(ts', fss)	= unzip $ map (stripFWheresT justMono) ts
	    in	( TSum k ts'
	    	, concat fss)

	TApp t1 t2
	 -> let	(t1', fs1)	= stripFWheresT justMono t1
	 	(t2', fs2)	= stripFWheresT justMono t2
	    in	( TApp t1' t2'
	    	, fs1 ++ fs2)
		
	TCon tc	-> (tt, [])

 	TVar{}	-> (tt, [])
		
	-- closure
	TFree v t
	 -> let	(t', fs)	= stripFWheresT justMono t
	    in	( TFree v t'
	    	, fs)

	TDanger t1 t2
	 -> let (t1', fs1)	= stripFWheresT justMono t1
	  	(t2', fs2)	= stripFWheresT justMono t2
	    in	( TDanger t1' t2'
	        , fs1 ++ fs2)

	TClass k cid	-> (tt, [])

	_ -> panic stage
		$ "stripFWheresT: no match for " % tt % "\n"

isFWhere ff
 = case ff of
	FWhere _ _		-> True
	_			-> False

isMonoFWhere ff
 = case ff of
	FWhere TClass{} _	-> True
	_			-> False


