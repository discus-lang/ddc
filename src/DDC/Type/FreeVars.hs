{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | `freeVars` instance for types.
module DDC.Type.FreeVars
	()
where
import DDC.Type.Exp
import DDC.Var
import DDC.Util.FreeVars
import Data.Set			((\\), empty, union, unions, fromList, singleton)
import qualified Data.Set	as Set
import qualified Data.Map	as Map


-- Var ---------------------------------------------------------------------------------------------
instance FreeVars Var where
 freeVars v	= singleton v


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
 freeVars _	= empty

 
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
	 	\\ (fromList [ v | FWhere (TVar _ (UVar v)) _ <- fs])
			
	TConstrain t (Constraints { crsEq, crsMore, crsOther })
	 -> unions
		[ freeVars t
		, unions $ map freeVars $ Map.elems crsEq
		, unions $ map freeVars $ Map.keys  crsMore
		, unions $ map freeVars $ Map.elems crsMore
		, freeVars crsOther ]
		
	 	\\ (unions $ map freeVars $ Map.keys crsEq)
	
	TSum k ts	-> union (freeVars k)  (freeVars ts)
	TApp t1 t2	-> union (freeVars t1) (freeVars t2)
	TCon tycon	-> freeVars tycon

 	TVar k (UVar v)	-> union (freeVars k)  (singleton v)

	TVar k (UMore v t)
	 -> unions
	 	[ freeVars k
		, Set.singleton v
		, freeVars t]
	
	TVar k UIndex{}	-> freeVars k
	TVar k UClass{}	-> freeVars k

	TError{}	-> empty
	
-- Bind -------------------------------------------------------------------------------------------
instance FreeVars Bind where
 freeVars bb
  = case bb of
	BNil		-> Set.empty
	BVar _		-> Set.empty
	BMore _ t	-> freeVars t

-- Fetter ------------------------------------------------------------------------------------------
instance FreeVars Fetter where
 freeVars f
  = case f of
	FConstraint v ts	
	 -> union (singleton v) (freeVars ts)

	FWhere (TVar k (UVar v)) t2
	 -> union (freeVars k)
	 	  (freeVars t2 \\ singleton v)

	FWhere t1 t2
	 -> union (freeVars t1) (freeVars t2)
		
	FMore t1 t2
	 -> union (freeVars t1) (freeVars t2)

	FProj _ v tDict tBind
	 -> unions
	 	[ singleton v
		, freeVars tDict
		, freeVars tBind]

