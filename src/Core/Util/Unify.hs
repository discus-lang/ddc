
module Core.Util.Unify
	( unifyT2
	, matchC2)

where

import Util
import Shared.Error

import Core.Exp
import Core.Pretty
import Core.Util.Bits

stage	= "Core.Util.Unify"


-- Unify two types
--	effects regions and closures always unify
--	types unify if they have the same data structure
--

unifyT2 :: Type -> Type -> Maybe [(Type, Type)]
unifyT2 t1 t2
	| TData v1 ts1			<- t1
	, TData v2 ts2			<- t2
	, v1 == v2
	= liftM concat 
		$ sequence 
		$ zipWith unifyT2 ts1 ts2

	| TFunEC a1 b1 eff1 clo1	<- t1
	, TFunEC a2 b2 eff2 clo2	<- t2
	, Just subA		<- unifyT2 a1 a2
	, Just subB		<- unifyT2 b1 b2
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
	| KClass{}			<- kindOfType t1
	, KClass{}			<- kindOfType t2
	= Just [(t1, t2)]
	
	| elem (kindOfType t1) [KRegion, KEffect, KClosure]
	, kindOfType t1 == kindOfType t2
	= Just [(t1, t2)]

	-- wildcards
	| TWild k		<- t1		
	, kindOfType t2 == k		= Just [(t1, t2)]
	
	| TWild k		<- t2
	, kindOfType t1 == k		= Just [(t1, t2)]

	| otherwise			= Nothing
	
	
-- Check if two classes match
--	they match if they have the same class constructor name, 
--	and their arguments unify
--
matchC2	:: Class -> Class -> Bool
matchC2 c1 c2
 	| TClass v1 ts1	<- c1
	, TClass v2 ts2	<- c2
	, length ts1	== length ts2
	, v1 == v2
	, and $ map isJust (zipWith unifyT2 ts1 ts2)
	= True
	
	| otherwise
	= False
	



