 
module Type.Check.Danger
	(dangerousCidsT)

where

import Util

import qualified Shared.Var	as Var
import qualified Shared.VarBind	as Var
import Shared.VarPrim

import Type.Exp
import Type.Error
import Type.Util
import Type.Plate.Collect
import Type.State
import Type.Class

import Shared.Error

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Data.Map	as Map
import Data.Map			(Map)

-----
stage	= "Type.Check.Danger"
-- debug	= True
-- trace s = when debug $ traceM s


dangerousCidsT :: Type -> [ClassId]
dangerousCidsT tt
 = let	tsDanger	= dangerT Set.empty Map.empty tt
   in	[ cid	| TClass k cid	<- Set.toList tsDanger
	    		, elem k [KValue, KEffect, KClosure] ]
	    

dangerT 
	:: Set Type
	-> Map Type Type
	-> Type -> Set Type

dangerT rsMutable fsClosure tt
 = case tt of
 	TVar{}			-> Set.empty
	TClass{}		-> Set.empty

	TForall vks t		
	 -> dangerT rsMutable fsClosure t

	-- fetters
	TFetters fs t1
	 ->     -- remember any regions flagged as mutable
	    let	rsMoreMutable	= Set.fromList
	 			$ [r	| FConstraint v [r]	<- fs
					, Var.bind v 	== Var.FMutable ]

		rsMutable'	= Set.union rsMutable rsMoreMutable

		-- collect up more closure bindings
		fsClosure'	= Map.union 
					fsClosure 
					(Map.fromList [(u1, u2) | FLet u1 u2	<- fs
								, takeKindOfType u1 == Just KClosure])

		-- decend into type and fetters
		t1Danger	= dangerT rsMutable' fsClosure' t1

	    in	t1Danger
	    
	    
	-- functions
	TFun t1 t2 eff clo	
	 -> let cloDanger	
	 		| TBot KClosure	<- clo
			= Set.empty

			| otherwise
			= case Map.lookup clo fsClosure of
				Just c	-> dangerT rsMutable fsClosure c
				Nothing	-> Set.empty
			
	    in	Set.unions
			[ cloDanger ]

	-- data constructors
	TApp{}	
	 -> case takeTData tt of
	 	Just (v, k, ts)	-> dangerT_data rsMutable fsClosure (v, k, ts)
		_		-> Set.empty

	TData k v ts		-> dangerT_data rsMutable fsClosure (v, k, ts)

	-- closures
	TFree v t
	 -> dangerT rsMutable fsClosure t

	TDanger t1 t2
	 	|  Set.member t1 rsMutable
	 	-> Set.fromList $ collectTClassVars t2

		| otherwise
		-> dangerT rsMutable fsClosure t2

	TSum KClosure ts
	 -> Set.unions $ map (dangerT rsMutable fsClosure) ts

	TMask KClosure t v
	 -> dangerT rsMutable fsClosure t

	-- effects
	TSum KEffect ts
	 -> Set.empty

	TEffect{}
	 -> Set.empty


	-- skip over errors
	TError{}		-> Set.empty
	 
	_ -> panic stage
		$ "dangerT: no match for " % tt
	 

dangerT_data rsMutable fsClosure (v, k, ts)
	-- if this ctor has any mutable regions then all vars from this point down are dangerous
 	| or $ map 	(\t -> case t of 
 				TVar{}		-> Set.member t rsMutable
				TClass{}	-> Set.member t rsMutable
				_		-> False)
			ts

	= Set.unions $ map (Set.fromList . collectTClassVars) ts

	 -- check for dangerous vars in subterms
	| otherwise
 	= Set.unions $ map (dangerT rsMutable fsClosure) ts
