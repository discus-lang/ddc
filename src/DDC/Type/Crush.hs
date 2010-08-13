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

	 -- Deep Read
	 | TyConEffectDeepRead	<- tc
	 , t2'			<- crushT t2
	 -> case takeTData t2' of
		Just (_, _, ts)
		 -> let (tRs, tDs) = unzip $ map slurpTVarsRD ts
		    in  makeTSum kEffect
				(  [TApp tRead t	| t <- concat tRs]
				++ [TApp tDeepRead t	| t <- concat tDs] )

		Nothing	-> TApp t1 t2'

	 -- Deep Write
	 | TyConEffectDeepWrite	<- tc
	 , t2'			<- crushT t2
	 -> case takeTData t2' of
		Just (_, _, ts)
		 -> let (tRs, tDs) = unzip $ map slurpTVarsRD ts
		    in  makeTSum kEffect
				(  [TApp tWrite t	| t <- concat tRs]
				++ [TApp tDeepWrite t	| t <- concat tDs] )

		Nothing	-> TApp t1 t2'
	
	TApp t1 t2
	 -> TApp (crushT t1) (crushT t2)
	
	TForall b k t
	 -> TForall b k (crushT t)
		
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

	-- Head Lazy
	KApp kc t2
	 | kc == kHeadLazy 
	 , Just (_, _, TVar kR r : _)	<- takeTData t2
	 , kR == kRegion
	 -> KApp kLazy (TVar kR r)

	-- Deep Mutable
	KApp kc t2
	 | kc == kDeepMutable
	 , Just _		<- takeTData t2
	 , (tsRegion, tsData)	<- slurpTVarsRD t2
	 -> makeKSum 
		$  map (KApp kMutable)     tsRegion
		++ map (KApp kDeepMutable) tsData

	-- Deep Const
	KApp kc t2
	 | kc == kDeepConst
	 , Just _		<- takeTData t2
	 , (tsRegion, tsData)	<- slurpTVarsRD t2
	 -> makeKSum 
		$  map (KApp kConst)     tsRegion
		++ map (KApp kDeepConst) tsData
	
	KApp k1 t2
	 -> KApp (crushK k1) (crushT t2)
	
	KSum ks
	 -> makeKSum $ map crushK ks



