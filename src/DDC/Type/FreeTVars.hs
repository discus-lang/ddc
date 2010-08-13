
{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Collect FreeVars in some thing.
--   We get the TVars containing `UVar` and `UMore` nodes that have free Vars
--   and _all_ of the TVars with containing `UIndex` and `UClass` nodes.
--
module DDC.Type.FreeTVars
	( freeTClasses
	, freeTClassVars
	, freeCids
	, freeTVars)
where
import DDC.Type.Exp
import DDC.Type.Predicates
import Data.Set			(Set)
import qualified Data.Map 	as Map
import qualified Data.Set 	as Set


-- | Collect TClasses that appear in bound positions in this thing.
freeTClasses :: FreeTVars a => a -> Set Type
freeTClasses xx 
	= Set.filter isTClass $ freeTVars xx


-- | Collect TClasses and free TVars that appear in bound positions in this thing.
freeTClassVars :: FreeTVars a => a -> Set Type
freeTClassVars xx
	= Set.filter (\x -> isTClass x || isSomeTVar x)
	$ freeTVars xx


-- | Collect cids that appear in bound positions in this thing.
freeCids :: FreeTVars a => a -> Set ClassId
freeCids xx
	= Set.map (\(TVar _ (UClass cid)) -> cid)
	$ Set.filter isTClass
	$ freeTVars xx


class FreeTVars a where
	-- | Collect free TVars in this thing.
	freeTVars :: a -> Set Type

instance FreeTVars a => FreeTVars [a] where
 freeTVars ts	= Set.unions $ map freeTVars ts

instance FreeTVars Kind where
 freeTVars _	= Set.empty

instance FreeTVars Type where
 freeTVars tt
  = case tt of
	TNil		-> Set.empty
	TVar{}		-> Set.singleton tt
	TCon{}		-> Set.empty

	TSum k ts		
	 -> Set.unions 
		(freeTVars k : map freeTVars ts)

	TApp t1 t2		
	 -> Set.union  (freeTVars t1) (freeTVars t2)

	TForall BNil k t
	 -> Set.union 
		(freeTVars k)
		(freeTVars t)
	
	TForall (BVar v) k t
	 -> (Set.unions 
	 	[ freeTVars k
		, freeTVars t]) 	
			Set.\\ Set.singleton (TVar k (UVar v))
		
	TForall (BMore v t1) k t2
	 -> (Set.unions 
	 	[ freeTVars t1
		, freeTVars k
		, freeTVars t2])
			Set.\\ Set.singleton (TVar k (UMore v t1))

	TConstrain t crs
	 -> Set.union  (freeTVars t)  (freeTVars crs)

	TError{}
	 -> Set.empty


instance FreeTVars Fetter where
 freeTVars ff
  = case ff of
	FConstraint _ ts	
	 -> Set.unions 
	  $ map freeTVars ts

	FWhere _ t2
	 -> freeTVars t2

	FMore  _ t2
	 -> freeTVars t2

	FProj  _ _ t1 t2
	 -> Set.union (freeTVars t1) (freeTVars t2)


instance FreeTVars Constraints where
 freeTVars crs
  = Set.unions 
	[ Map.fold (Set.union . freeTVars) Set.empty (crsEq   crs)
	, Map.fold (Set.union . freeTVars) Set.empty (crsMore crs)
	, Set.unions (map freeTVars $ crsOther crs) ]




