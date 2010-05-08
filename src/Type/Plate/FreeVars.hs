{-# OPTIONS -fwarn-incomplete-patterns #-}

module Type.Plate.FreeVars
	(FreeVars(..))
where
import Type.Exp
import Shared.FreeVars
import DDC.Var
import Data.Set			((\\), empty, union, unions, fromList, singleton)
import qualified Data.Set	as Set
import qualified Data.Map	as Map


-- Var ---------------------------------------------------------------------------------------------
instance FreeVars Var where
 freeVars v	= singleton v

 
-- Type --------------------------------------------------------------------------------------------
instance FreeVars Type where
 freeVars tt
  = case tt of
	TNil
	 -> empty

	TForall (BVar v) k t
	 -> (unions 
	 	[ freeVars k
		, freeVars t]) 	\\ singleton v

	TForall (BMore v t1) k t2
	 -> (unions 
	 	[ freeVars t1
		, freeVars k
		, freeVars t2])	\\ singleton v
	 
	TContext k t
	 -> union (freeVars k) (freeVars t)

	TFetters t fs
	 -> union (freeVars fs) (freeVars t)
	 	\\ (fromList [ v | FWhere (TVar k v) _ <- fs])
			
	TConstrain t (Constraints { crsEq, crsMore, crsOther })
	 -> unions
		[ freeVars t
		, unions $ map freeVars $ Map.elems crsEq
		, unions $ map freeVars $ Map.keys  crsMore
		, unions $ map freeVars $ Map.elems crsMore
		, freeVars crsOther ]
		
	 	\\ (unions $ map freeVars $ Map.keys crsEq)
	
	
	TSum k ts	-> freeVars ts
	 
	TApp t1 t2	-> union (freeVars t1) (freeVars t2)
	
	TCon tycon	-> freeVars tycon
	
 	TVar k v	-> singleton v

	-- effect
	TEffect v ts
	 -> union (singleton v) (freeVars ts)
	 
	-- closure
	TFree v t	-> freeVars t

	TDanger t1 t2	
	 -> unions 
	 	[ freeVars t1
		, freeVars t2]

	-- used in solver
	TClass{}	-> empty
	TError{}	-> empty
	 
	-- sugar
	TElaborate ee t	-> freeVars t
	
	-- core stuff
	TVarMore k v t
	 -> unions
	 	[ Set.singleton v
		, freeVars t]
	
	TIndex{}	-> empty
	

-- TyCon -------------------------------------------------------------------------------------------
instance FreeVars TyCon where
 freeVars tt
  = case tt of
  	TyConFun{}			-> Set.empty
	TyConData    { tyConName }	-> Set.singleton tyConName
	TyConWitness { }		-> Set.empty


-- Kind --------------------------------------------------------------------------------------------
instance FreeVars Kind where
 freeVars kk	= empty
	

-- Fetter ------------------------------------------------------------------------------------------
instance FreeVars Fetter where
 freeVars f
  = case f of
	FConstraint v ts	
	 -> union (singleton v) (freeVars ts)

	FWhere (TVar k v) t2
	 -> freeVars t2
	 	\\ singleton v

	FWhere t1 t2
	 -> union (freeVars t1) (freeVars t2)
		
	FMore t1 t2
	 -> union (freeVars t1) (freeVars t2)

	FProj pj v tDict tBind
	 -> unions
	 	[ singleton v
		, freeVars tDict
		, freeVars tBind]




