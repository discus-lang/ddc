
-- | Collect classids that appear in bound positions in some thing.
module DDC.Type.FreeCids
	(freeCids)
where
import DDC.Type.Exp
import Data.Set		(Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

class FreeCids a where
	-- | Collect classids that appear in bound positions in this thing.
	freeCids :: a -> Set ClassId

instance FreeCids Type where
 freeCids tt
  = case tt of
	TNil			-> Set.empty
	TVar _ u		-> freeCids u
	TCon{}			-> Set.empty
	TSum _ ts		-> Set.unions $ map freeCids ts
	TApp t1 t2		-> Set.union  (freeCids t1) (freeCids t2)
	TForall _ _ t		-> freeCids t
	TFetters t fs		-> Set.unions (freeCids t : map freeCids fs)
	TConstrain t crs	-> Set.union  (freeCids t)  (freeCids crs)
	TError{}		-> Set.empty


instance FreeCids Bound where
 freeCids bb
  = case bb of
	UClass cid		-> Set.singleton cid
	_			-> Set.empty


instance FreeCids Fetter where
 freeCids ff
  = case ff of
	FConstraint _ ts	-> Set.unions $ map freeCids ts
	FWhere _ t2		-> freeCids t2
	FMore  _ t2		-> freeCids t2
	FProj  _ _ t1 t2	-> Set.union (freeCids t1) (freeCids t2)


instance FreeCids Constraints where
 freeCids crs
  = Set.unions 
	[ Map.fold (Set.union . freeCids) Set.empty (crsEq   crs)
	, Map.fold (Set.union . freeCids) Set.empty (crsMore crs)
	, Set.unions (map freeCids $ crsOther crs) ]
		
	
