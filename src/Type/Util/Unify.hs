
-- | Unification of types.

module Type.Util.Unify
	( unifyT2 )

where

import Type.Exp
import Type.Util.Bits

import Util


-- Unify two types
--	effects regions and closures always unify
--	types unify if they have the same data structure
--
unifyT2 :: Type -> Type -> Maybe [(Type, Type)]
unifyT2 t1 t2
	-- data
	| TData v1 ts1			<- t1
	, TData v2 ts2			<- t2
	, v1 == v2
	= liftM concat 
		$ sequence 
		$ zipWith unifyT2 ts1 ts2

	| TFun a1 b1 eff1 clo1		<- t1
	, TFun a2 b2 eff2 clo2		<- t2
	, Just subA			<- unifyT2 a1 a2
	, Just subB			<- unifyT2 b1 b2
	= Just (subA ++ subB)

	-- vars
	| TVar k1 v1			<- t1
	, TVar k2 v2			<- t2
	, k1 == k2
	, v1 == v2			= Just []
	
	| TVar k1 v1			<- t1	
	, kindOfType t2 == k1		= Just [(t1, t2)]
	
	| TVar k2 v2			<- t2
	, kindOfType t1 == k2		= Just [(t1, t2)]

	-- regions, effects, closures and classes always unify
	| elem (kindOfType t1) [KRegion, KEffect, KClosure]
	, kindOfType t1 == kindOfType t2
	= Just [(t1, t2)]

	-- wildcards
	| TWild k		<- t1		
	, kindOfType t2 == k		= Just [(t1, t2)]
	
	| TWild k		<- t2
	, kindOfType t1 == k		= Just [(t1, t2)]

	| otherwise			= Nothing
