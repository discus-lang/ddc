{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- | Crush compound effects and fetters into their components.
module DDC.Type.Crush
	( crushT
	, crushK)
where
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Builtin
import DDC.Type.Kind
import Type.Util	(slurpTVarsRD)


-- | Crush effects and constraints in a type.
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
	

-- | Crush effects and constraints in a kind.
crushK :: Kind -> Kind
crushK kk
 = case kk of
	KNil{}		-> kk
	KCon{}		-> kk

	KFun k1 k2
	 -> KFun (crushK k1) (crushK k2)
	
	-- Deep Mutable
	KApp kc t2
	 | kc == kDeepMutable
	 , Just _		<- takeTData t2
	 , (tsRegion, tsData)	<- slurpTVarsRD t2
	 -> makeKSum 
		$  map (KApp kMutable)     tsRegion
		++ map (KApp kDeepMutable) tsData
	
	KApp k1 t2
	 -> KApp (crushK k1) (crushT t2)
	
	KSum ks
	 -> makeKSum $ map crushK ks



