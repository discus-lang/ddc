
module Type.Check.Soundness
	( --checkUpdateSoundness
	  dangerousCidsT)

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
stage	= "Type.Soundness"
debug	= True
trace s = when debug $ traceM s


-- checkUpdateSoundness
--	Check for update soudness problems in an already generalised scheme.
--
--	The type inferencer might find out that some scheme has mutable components
--	after it's already generalised and instantiated it several times. It's 
--	too late to apply monomorphism restrictions, but we can at least report
--	an error.
--
--	If you want the monomorphism restriction, and not the error, then supply
--	a type sig for the scheme including an appropriate mutability constraint.
--
{-
checkUpdateSoundness
	:: Var -> Type -> SquidM ()
	
checkUpdateSoundness varT t
 = do	
	let ?fsMutable
		= nub
		$ [f 	| f@(FClass v _) <- collectFetters t
			, Var.bind v == Var.FMutable]

	let dangerTs
		= nub
		$ [t	| t@(TVar v)	<- dangerT [] t]
 
 	trace	$ "*   CheckUpdate.checkUpdate " 	% t 	% "\n"
		% "    fsMutable = " % ?fsMutable		% "\n"
		% "    dangerTs  = " % dangerTs			% "\n"
		% "\n"
 	
	when (not $ isNil dangerTs)
	 $ do
	 	addErrors 
			[ErrorUpdateSoundness 
				{ eVar		= varT
				, eType		= t
				, eTypeDanger	= dangerTs }]
		
	return	()
 
-----
-}


dangerousCidsT :: Type -> [ClassId]
dangerousCidsT tt
 = let	tsDanger	= dangerT Set.empty Map.empty tt
   in	[ cid	| TClass k cid	<- Set.toList tsDanger
	    		, elem k [KData, KEffect, KClosure] ]
	    

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
								, kindOfType u1 == KClosure])

		-- decend into type and fetters
		t1Danger	= dangerT rsMutable' fsClosure' t1

		fsDanger	= Set.unions 
				$ map (dangerT rsMutable' fsClosure') 
					[ u2	| FLet u1 u2	<- fs
						, kindOfType u1 == KClosure]	

	    in	Set.union t1Danger fsDanger
	    
	    
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
			[ dangerT rsMutable fsClosure t1
			, dangerT rsMutable fsClosure t2 
			, cloDanger ]

	-- data constructors
	TData v ts
		-- if this ctor has any mutable regions then all vars from this point down are dangerous
	 	| or $ map 	(\t -> case t of 
	 				TVar{}		-> Set.member t rsMutable
					TClass{}	-> Set.member t rsMutable
					_		-> False)
				ts

		-> Set.unions $ map (Set.fromList . collectTClassVars) ts

		 -- check for dangerous vars in subterms
		| otherwise
	 	-> Set.unions $ map (dangerT rsMutable fsClosure) ts

	-- closures
	TFree v t
	 -> dangerT rsMutable fsClosure t

	TSum KClosure ts
	 -> Set.unions $ map (dangerT rsMutable fsClosure) ts

	TMask KClosure t v
	 -> dangerT rsMutable fsClosure t

	-- skip over errors
	TError{}		-> Set.empty
	 
	_ -> panic stage
		$ "dangerT: no match for " % tt
	 
	 
	 
	 
	 
	 
	  	
