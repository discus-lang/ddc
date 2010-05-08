
module Type.Effect.MaskLocal
	( maskLocalT 
	, visibleRsT )
where
import Util
import Type.Exp
import Type.Builtin
import Type.Util
import Shared.VarPrim
import DDC.Main.Error
import qualified Data.Set	as Set

-----
stage	= "Type.Effect.MaskLocal"

-- | Mask effects on local regions.
-- 	At generalisation time, if a region is not present in the type or closure of a
-- 	function then is local to that function and all effects involving that region 
-- 	can be erased from the type.
--
--	We can also erase Const/Mutable Lazy/Direct constraints because these will be
--	fulfilled by the letregion construct used to locally create the region.
--
maskLocalT :: Set Type -> Type -> Type
maskLocalT tsVis tt
 = case tt of
	TForall  b k t1		-> TForall b k (maskLocalT tsVis t1)
	TFetters t1 fs		-> addFetters (catMaybes $ map (maskF tsVis) fs) t1
	_ 			-> tt


-- | Erase read effects to regions not in this list.
--	also erase Const, Mutable, Lazy, Direct constraints on regions not in the list

maskF :: Set Type -> Fetter -> Maybe Fetter

maskF	tsVis	(FWhere t1 t2)
	| kindOfType t1 == Just kEffect
	= Just $ FWhere t1 (maskE tsVis t2)

maskF	tsVis	(FConstraint v [tR])
	| elem v [primConst, primMutable, primLazy, primDirect]
	, isTClass tR || isSomeTVar tR
	, not $ Set.member tR tsVis
	= Nothing
	
maskF	tsVis	f	
	= Just f


-- | Erase read effects to regions not in this list.
maskE :: Set Type -> Effect -> Effect
maskE	 tsVis	eff
	= makeTSum kEffect 
	$ catMaybes $ map (maskE' tsVis) 
	$ flattenTSum eff 

	
maskE'	tsVis eff

	| TEffect v [tR]	<- eff
	, elem v [primRead, primWrite]
	, isTClass tR || isSomeTVar tR
	, not $ Set.member tR tsVis
	= Nothing
	
	| otherwise
	= Just eff



-- | Collect the list of visible regions from the type sig. 
--   We can't just call freeVarsT, because we don't want to get
--   region vars present in the effect portion of the type.
--
visibleRsT :: Type -> Set Type
visibleRsT tt
 = case tt of
	TForall b k t		-> visibleRsT t
	TFetters t fs		-> visibleRsT t

	TSum k ts		-> Set.unions $ map visibleRsT ts
	 
	TVar kR _		
	 | kR	== kRegion	-> Set.singleton tt

	TVar{}			-> Set.empty
	TCon{}			-> Set.empty

	-- data
	TApp t1 t2
	 -> Set.unions
		[ visibleRsT t1
		, visibleRsT t2 ]

	TEffect{}		-> Set.empty	

	-- closure
	TFree v t		-> visibleRsT t
	TDanger t1 t2		-> Set.union (visibleRsT t1) (visibleRsT t2)
	 
	TClass kR cid	
	 | kR	== kRegion	-> Set.singleton tt


	TClass{}		-> Set.empty
	TError{}		-> Set.empty	
	_
	 -> panic stage
	 	$ "visibleRsT: no match for " % tt
	 
	
