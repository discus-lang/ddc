
module Type.Effect.MaskLocal
	( maskLocalT 
	, visibleRsT )

where

-----
import Util


import Type.Exp
import Type.Plate
import Type.Util

import Shared.Error
import Shared.VarPrim
import qualified Shared.Var	as Var
import Shared.Var		(NameSpace (..))

import qualified Data.Set	as Set
import Data.Set			(Set)

-----
stage	= "Type.Effect.MaskLocal"

-- | Mask effects on local regions.
--	
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
	TForall  vks t1		-> TForall vks (maskLocalT tsVis t1)
	TFetters fs  t1		-> addFetters (catMaybes $ map (maskF tsVis) fs) t1
	_ 			-> tt


-- | Erase read effects to regions not in this list.
--	also erase Const, Mutable, Lazy, Direct constraints on regions not in the list

maskF :: Set Type -> Fetter -> Maybe Fetter

maskF	tsVis	(FLet t1 t2)
	| kindOfType t1 == KEffect
	= Just $ FLet t1 (maskE tsVis t2)

maskF	tsVis	(FConstraint v [tR])
	| elem v [primConst, primMutable, primLazy, primDirect]
	, tR =@= TClass{} || tR =@= TVar{}
	, not $ Set.member tR tsVis
	= Nothing
	
maskF	tsVis	f	
	= Just f


-- | Erase read effects to regions not in this list.
maskE :: Set Type -> Effect -> Effect
maskE	 tsVis	eff
	= makeTSum KEffect 
	$ catMaybes $ map (maskE' tsVis) 
	$ flattenTSum eff 

	
maskE'	tsVis eff

	| TEffect v [tR]	<- eff
	, elem v [primRead, primWrite]
	, tR =@= TClass{} || tR =@= TVar{}
	, not $ Set.member tR tsVis
	= Nothing
	
	| otherwise
	= Just eff



-----------------------
-- visRegions
--	Collect the list of visible regions from the type sig. 
--	We can't just call freeVarsT, because we don't want to get
--		region vars present in the effect portion of the type.
--
visibleRsT :: Type -> Set Type
visibleRsT tt
 = case tt of
	TForall vks t
	 -> visibleRsT t
	 
	TFetters fs t
	 -> visibleRsT t

	TSum k ts
	 -> Set.unions $ map visibleRsT ts
	 
	TMask k t1 t2
	 -> Set.unions
	 	[ visibleRsT t1 ]
		
	TVar KRegion _		-> Set.singleton tt
	TVar{}			-> Set.empty

	TTop{}	-> Set.empty
	TBot{}	-> Set.empty

	-- data
	TData v ts
	 -> Set.unions $ map visibleRsT ts

 	TFun t1 t2 eff clo	
	 -> Set.unions
	 	[ visibleRsT t1
		, visibleRsT t2
		, visibleRsT clo ]

	-- 
	TEffect{}		-> Set.empty	

	-- closure
	TFree v t		-> visibleRsT t
	TDanger t1 t2		-> Set.union (visibleRsT t1) (visibleRsT t2)
	 
	TClass KRegion cid	-> Set.singleton tt
	TClass{}		-> Set.empty
	
	TError{}		-> Set.empty	

	_
	 -> panic stage
	 	$ "visibleRsT: no match for " % tt
	 
	
