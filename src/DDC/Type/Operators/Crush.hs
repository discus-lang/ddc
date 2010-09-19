{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Crush compound effects and fetters into their components.
module DDC.Type.Operators.Crush
	( crushT
	, crushK)
where
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Builtin
import DDC.Type.Kind
import DDC.Type.Data.Pretty	()
import DDC.Type.Data
import Type.Util		(slurpTVarsRD)
import Data.Maybe

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

	 -- Deep reads and writes only apply to the material parameters of a type constructor.
	 --  If there is a data def attached to the TyCon then we can use that to determine
	 --  the materiality. If there is no def, then this is probably a foreign imported type
	 --  so we assume all the params are strongly material.
	 -- 
	 --  If the data def has gone missing for some reason (the field on the TyCon is Nothing)
	 --  then we just get all the possible effects, which is a safe default behaviour.
			
	 -- Deep Read
	 | TyConEffectDeepRead	<- tc
	 , t2'			<- crushT t2
	 -> case takeTDataTC t2' of
		Just (tcData, ts)
		 -> let	mats		= fromMaybe (replicate (length ts) MaterialStrong)
					$ do 	def	<- tyConDataDef tcData
						paramMaterialityOfDataDef def
						
			tsMaterial	= [t	| t <- ts 
						| m <- mats
						, m == MaterialStrong || m == MaterialMixed]
						
			newReads	= [TApp tRead t		| t <- tsMaterial, isRegion t ]
			newDeepReads	= [TApp tDeepRead t	| t <- tsMaterial, isValueType t]
			
		    in	makeTSum kEffect (newReads ++ newDeepReads)

		Nothing	-> TApp t1 t2'

	 -- Deep Write
	 | TyConEffectDeepWrite	<- tc
	 , t2'			<- crushT t2
	 -> case takeTDataTC t2' of
		Just (tcData, ts)
		 -> let	mats		= fromMaybe (replicate (length ts) MaterialStrong)
					$ do 	def	<- tyConDataDef tcData
						paramMaterialityOfDataDef def
						
			tsMaterial	= [t	| t <- ts 
						| m <- mats
						, m == MaterialStrong || m == MaterialMixed]
						
			newWrites	= [TApp tWrite t	| t <- tsMaterial, isRegion t ]
			newDeepWrites	= [TApp tDeepWrite t	| t <- tsMaterial, isValueType t]
			
		    in	makeTSum kEffect (newWrites ++ newDeepWrites)
		
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



