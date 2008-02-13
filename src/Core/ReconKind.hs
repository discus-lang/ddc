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

	TBot k			-> k
	TTop k			-> k

 	TData{}			-> KData
	TFunEC{}		-> KData
	TFun{}			-> KData
	
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

