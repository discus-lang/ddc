
module Type.Merge
(
--	mergeEffectsT
)

where

import Util

import Type.Exp
import Type.Util

-----------------------
-- mergeEffectsT
--	Merges effect fetters into the body of the type.
--
--	eg,   Int %r1 -(!e1)>      Int %r1 :- !e1 = { Read %r1 }
--	=>    Int %r1 -{Read %r1}> Int %r1
--

{-
mergeEffectsT :: Type -> Type
mergeEffectsT t
	= packForallVars
	$ liftFetters 
	$ mergeEffects' [] t

mergeEffects' fs t
 = case t of
 	TForall  vks t	
	 -> let	t'	= mergeEffects' fs t
	    in	TForall vks t'

	TFetters fs2 t	
	 -> let t'	= mergeEffects' (nub $ fs ++ fs2) t
		fs2'	= filter (\f -> not $ isFEffect f) fs2
	    in	TFetters fs2' t'
	    
	TFun   t1 t2 eff clo
	 -> let t2'	= mergeEffects' fs t2
	 	eff'	= case eff of
				EVar eV -> fromMaybe eff
					 $ lookupEffFs eV fs
				_	-> eff

	    in	TFun t1 t2' eff' clo

	TCon v ts
	 -> let	ts'	= map (mergeEffects' fs) ts
	    in	TCon v ts'
	    
	TClass{}	-> t

	TVar{}		-> t
	TRegion{} 	-> t
	TEffect{}	-> t
	

lookupEffFs v ff
 = case ff of
	[]		-> Nothing

 	(FEffect (EVar v') effs : fs)
	 | v == v'	-> Just effs
	 
	(f:fs) 		-> lookupEffFs v fs

-}
