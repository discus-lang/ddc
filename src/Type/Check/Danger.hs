 
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
{-
debug	= True
trace s = when debug $ traceM s
-}

dangerousCidsT :: Type -> [ClassId]
dangerousCidsT tt
 = let	tsDanger	= dangerT Set.empty Map.empty tt
   in	[ cid	| TClass k cid	<- Set.toList tsDanger
	    		, elem k [kValue, kEffect, kClosure] ]
	    

dangerT 
	:: Set Type
	-> Map Type Type
	-> Type -> Set Type

dangerT rsMutable fsClosure tt
 = case tt of
 	TVar{}			-> Set.empty
	TCon{}			-> Set.empty
	TClass{}		-> Set.empty

	TForall b k t		
	 -> dangerT rsMutable fsClosure t

	-- fetters
	TFetters t1 fs
	 ->     -- remember any regions flagged as mutable
	    let	rsMoreMutable	= Set.fromList
	 			$ [r	| FConstraint v [r]	<- fs
					, Var.bind v 	== Var.FMutable ]

		rsMutable'	= Set.union rsMutable rsMoreMutable

		-- collect up more closure bindings
		fsClosure'	= Map.union 
					fsClosure 
					(Map.fromList [(u1, u2) | FWhere u1 u2	<- fs
								, kindOfType u1 == Just kClosure])

		-- decend into type and fetters
		t1Danger	= dangerT rsMutable' fsClosure' t1

	    in	t1Danger
	    
	TConstrain{}
	 -> dangerT rsMutable fsClosure
	  $ toFetterFormT tt
	    
	TApp{}
	 | Just (t1, t2, eff, clo)	<- takeTFun tt
	 -> let cloDanger	
	 		| TBot kC <- clo
			, kC	== kClosure
			= Set.empty

			| otherwise
			= case Map.lookup clo fsClosure of
				Just c	-> dangerT rsMutable fsClosure c
				Nothing	-> Set.empty
			
	    in	Set.unions
			[ cloDanger ]

   	 | Just (v, k, ts)		<- takeTData tt
	 -> dangerT_data rsMutable fsClosure (v, k, ts)
	 
	 | otherwise
	 -> Set.empty

	-- closures
	TFree v t
	 -> dangerT rsMutable fsClosure t

	TDanger t1 t2
	 	|  Set.member t1 rsMutable
	 	-> collectTClassVars t2

		| otherwise
		-> dangerT rsMutable fsClosure t2

	TSum kC ts
	 | kC	== kClosure
	 -> Set.unions $ map (dangerT rsMutable fsClosure) ts


	-- effects
	TSum kE ts
	 | kE	== kEffect	-> Set.empty

	TEffect{}		-> Set.empty

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

	= Set.unions $ map collectTClassVars ts

	 -- check for dangerous vars in subterms
	| otherwise
 	= Set.unions $ map (dangerT rsMutable fsClosure) ts
