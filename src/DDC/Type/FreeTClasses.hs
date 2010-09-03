{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Collect TClasses that appear in bound positions in some thing.
module DDC.Type.FreeTClasses
	(freeTClasses)
where
import DDC.Type.Exp
import Data.Set		(Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

class FreeTClasses a where
	-- | Collect classids that appear in bound positions in this thing.
	freeTClasses :: a -> Set Type

instance FreeTClasses Type where
 freeTClasses tt
  = case tt of
	TNil			-> Set.empty
	
	TVar _ (UClass _)	-> Set.singleton tt
	TVar{}			-> Set.empty
	
	TCon{}			-> Set.empty
	TSum _ ts		-> Set.unions $ map freeTClasses ts
	TApp t1 t2		-> Set.union  (freeTClasses t1) (freeTClasses t2)
	TForall _ _ t		-> freeTClasses t
	TConstrain t crs	-> Set.union  (freeTClasses t)  (freeTClasses crs)
	TError{}		-> Set.empty


instance FreeTClasses Fetter where
 freeTClasses ff
  = case ff of
	FConstraint _ ts	-> Set.unions $ map freeTClasses ts
	FWhere _ t2		-> freeTClasses t2
	FMore  _ t2		-> freeTClasses t2
	FProj  _ _ t1 t2	-> Set.union (freeTClasses t1) (freeTClasses t2)


instance FreeTClasses Constraints where
 freeTClasses crs
  = Set.unions 
	[ Map.fold (Set.union . freeTClasses) Set.empty (crsEq   crs)
	, Map.fold (Set.union . freeTClasses) Set.empty (crsMore crs)
	, Set.unions (map freeTClasses $ crsOther crs) ]
