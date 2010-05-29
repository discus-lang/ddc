{-# OPTIONS -fwarn-incomplete-patterns #-}

module Type.Util.StripFetters
	( stripFWheresT_all
	, stripFWheresT_mono )
where
import Util
import DDC.Type.Exp
import DDC.Main.Error
import Type.Pretty		()

stage	= "Type.Util.StripFetters"

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

	TConstrain{} -> panic stage $ "stripFWheresT: TConstrain"

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
		

isFWhere ff
 = case ff of
	FWhere _ _		-> True
	_			-> False

isMonoFWhere ff
 = case ff of
	FWhere (TVar _ UClass{}) _	-> True
	_				-> False


