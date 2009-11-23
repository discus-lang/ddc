
-- | Unification of types.

module Type.Util.Unify
	( unifyT2 )
where
import Type.Exp
import Type.Util.Bits
import Type.Util.Kind

import Util


-- Unify two types
--	effects regions and closures always unify
--	types unify if they have the same data structure
--
unifyT2 :: Type -> Type -> Maybe [(Type, Type)]
unifyT2 t1 t2
	| TApp t11 t12		<- t1
	, TApp t21 t22		<- t2
	, Just subA		<- unifyT2 t11 t21
	, Just subB		<- unifyT2 t12 t22
	= Just (subA ++ subB)

	| TCon tc1		<- t1
	, TCon tc2		<- t2
	, tc1 == tc2
	= Just []

	-- vars
	| TVar k1 v1		<- t1
	, TVar k2 v2		<- t2
	, k1 == k2
	, v1 == v2		= Just []
	
	| TVar _ v1		<- t1	
	, k1 == k2	
	= Just [(t1, t2)]
	
	| TVar _ v2		<- t2
	, k1 == k2
	= Just [(t1, t2)]

	-- regions, effects, closures and classes always unify
	| elem k1 [kRegion, kEffect, kClosure]
	, t1 == t2
	= Just [(t1, t2)]

	| otherwise			
	= Nothing

	where	Just k1	= kindOfType t1
		Just k2	= kindOfType t2
