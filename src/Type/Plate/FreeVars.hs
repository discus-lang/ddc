{-# OPTIONS -fwarn-incomplete-patterns #-}

module Type.Plate.FreeVars
	(FreeVars(..))
where
import DDC.Type.Exp
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
	TNil		-> empty

	TForall BNil k t
	 -> unions
		[ freeVars k
		, freeVars t ]

	TForall (BVar v) k t
	 -> (unions 
	 	[ freeVars k
		, freeVars t]) 	\\ singleton v

	TForall (BMore v t1) k t2
	 -> (unions 
	 	[ freeVars t1
		, freeVars k
		, freeVars t2])	\\ singleton v
	 
	TFetters t fs
	 -> union (freeVars fs) (freeVars t)
	 	\\ (fromList [ v | FWhere (TVar k (UVar v)) _ <- fs])
			
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
 	TVar k (UVar v)	-> singleton v

	TVar k (UMore v t)
	 -> unions
	 	[ Set.singleton v
		, freeVars t]
	
	TVar{}		-> empty

	TError{}	-> empty
	

-- TyCon -------------------------------------------------------------------------------------------
instance FreeVars TyCon where
 freeVars tt
  = case tt of
  	TyConFun{}			-> Set.empty
	TyConData    { tyConName }	-> Set.singleton tyConName

	TyConEffect (TyConEffectTop v) _ 
	 -> Set.singleton v
	
	TyConEffect{}			-> Set.empty

	-- BUGS: Do we really want to ignore the attached var?
	TyConClosure{}			-> Set.empty

	TyConWitness{}			-> Set.empty
	TyConElaborate{}		-> Set.empty

-- Kind --------------------------------------------------------------------------------------------
instance FreeVars Kind where
 freeVars kk	= empty
	

-- Fetter ------------------------------------------------------------------------------------------
instance FreeVars Fetter where
 freeVars f
  = case f of
	FConstraint v ts	
	 -> union (singleton v) (freeVars ts)

	FWhere (TVar k (UVar v)) t2
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




