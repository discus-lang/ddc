
module DDC.Type.FreeCids
	(freeCidsT)
where
import DDC.Type.Exp
import Data.Set		(Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Collect bound occurrences of cids in this type.
freeCidsT :: Type -> Set ClassId
freeCidsT tt
 = case tt of
	TNil			-> Set.empty
	TVar _ u		-> freeCidsU u
	TCon{}			-> Set.empty
	TSum _ ts		-> Set.unions $ map freeCidsT ts
	TApp t1 t2		-> Set.union  (freeCidsT t1) (freeCidsT t2)
	TForall _ _ t		-> freeCidsT t
	TFetters t fs		-> Set.unions (freeCidsT t : map freeCidsF fs)
	TConstrain t crs	-> Set.union  (freeCidsT t)  (freeCidsCRS crs)
	TError{}		-> Set.empty

	
freeCidsU :: Bound -> Set ClassId
freeCidsU bb
 = case bb of
	UClass cid		-> Set.singleton cid
	_			-> Set.empty


freeCidsF :: Fetter -> Set ClassId
freeCidsF ff
 = case ff of
	FConstraint _ ts	-> Set.unions $ map freeCidsT ts
	FWhere _ t2		-> freeCidsT t2
	FMore  _ t2		-> freeCidsT t2
	FProj  _ _ t1 t2	-> Set.union (freeCidsT t1) (freeCidsT t2)


freeCidsCRS :: Constraints -> Set ClassId
freeCidsCRS crs
	= Set.unions 
		[ Map.fold (Set.union . freeCidsT) Set.empty (crsEq   crs)
		, Map.fold (Set.union . freeCidsT) Set.empty (crsMore crs)
		, Set.unions (map freeCidsF $ crsOther crs) ]
		
	
