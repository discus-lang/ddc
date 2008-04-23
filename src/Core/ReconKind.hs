{-# OPTIONS -fwarn-incomplete-patterns #-}

module Core.ReconKind
	(kindOfType)
 where

import Core.Exp
import Core.Util.Bits
import Shared.VarPrim
import Shared.Error
import Util

stage	= "Core.ReconKind"

-- | Take the kind of a tycon
tyConKind :: TyCon -> Kind
tyConKind tyCon
 = case tyCon of
	TyConFun			-> KFun KValue (KFun KValue (KFun KEffect (KFun KClosure KValue)))
	TyConData { tyConDataKind }	-> tyConDataKind
	TyConClass { tyConClassKind }	-> tyConClassKind	 


	-- We don't give a real kind for TConPurify because its type rule requires
	--	the witness to purity to be of the same region as the effect to be purified.
	--	This is checked as a special case of TApp in kindOfType below, but we still want
	--	something to give for its 'kind' in dumps of the CoreIR
	
	--	W :: (Const r) |- purify (Read r) W :: (Pure (Read r))

	TyConPurify			-> KFun KEffect (KFun KSuper KSuper)

	TyConPureJoin { tyConAirity }	
	 -> let makeKind 0	= KSuper
	 	makeKind n	= KFun KSuper (makeKind (n-1))
	    in	makeKind tyConAirity
	 	
	
-- | Reconstruct the kind of this type, kind checking along the way
kindOfType :: Type -> Kind
kindOfType t
 = case t of
	TForall v t1 t2		-> kindOfType t2
	TContext t1 t2		-> kindOfType t2
	TFetters  t1 _		-> kindOfType t1

	TSum  k _		-> k
	TMask k _ _		-> k
	TVar  k _		-> k
	TVarMore k _ _		-> k

	TCon tyCon		-> tyConKind tyCon

	TBot k			-> k
	TTop k			-> k

	TApp t1 t2		
	 -> let result
		 	| KFun k11 k12	<- kindOfType t1
			, k2		<- kindOfType t2
			, k11 == k2
			= k12

			| otherwise
			= panic stage $ "kindOfType: kind error\n"
		
	   in	result	
	
	-- effect and closure constructors are always fully applied
	TEffect{}		-> KEffect
	TFree{}			-> KClosure

	-- witnesses
	-- BUGS: check the class is valid,
	--	eg one of Const, Mutable .. 
	TClass v ts		-> KClass v ts

	-- all the types being joined need to be purify witnesses
	TPurifyJoin ts
	 -> let	ks	= map kindOfType ts

		takePureEff (KClass v [eff])
			| v == primPure
			= eff
		
		takePureEff _
			= panic stage $ "kindOfType: takePureEff"
		
		effs	= map takePureEff ks
		
	    in	KClass primPure [makeTSum KEffect effs]

	TPurify eff@(TEffect vE [TVar KRegion vR1]) wit
		|  KClass vC [TVar KRegion vR2]	<- kindOfType wit
		,  vR1 == vR2
		,  vE  == primRead
		,  vC  == primConst
		-> KClass primPure [eff]
	
	TWitJoin ts
		-> makeKWitJoin (map kindOfType ts)

	TWild k			-> k
		
	_			-> panic stage $ "kindOfType: cannot get kind for " % show t % "\n"

