{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- | Crush compound effects and fetters into their components.
module DDC.Type.Crush
	(crushT)
where
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Builtin
import DDC.Type.Kind


crushT :: Type -> Type
crushT tt
 = case tt of
	TNil{}		-> tt
	TVar{}		-> tt
	TCon{}		-> tt
	TError{}	-> tt
		
	TSum k ts	
	 -> makeTSum k $ map crushT ts
	
	-- Head Read
	TApp t1@(TCon (TyConEffect tc _)) t2
	 | TyConEffectHeadRead 	<- tc
	 , t2'			<- crushT t2
	 -> case takeTData t2' of
		Just (_, _, (TVar kR r : _))
		 | isRegionKind kR
		 -> TApp tRead (TVar kR r)
		
		Just (_, _, [])
		  -> tPure
		
		_ -> TApp t1 t2'
	
	TApp t1 t2
	 -> TApp (crushT t1) (crushT t2)
	
	TForall b k t
	 -> TForall b k (crushT t)
	
	TFetters t fs
	 -> TFetters (crushT t) fs
	
	TConstrain t crs
	 -> TConstrain (crushT t) crs
	
