
-- | Replace classIds with their corresponding variables.
--	This is part of the process of type scheme generalisation.
module Type.Plug
	( plugClassIds
	, staticRsDataT
	, staticRsClosureT)	
where
import Type.Exp
import Type.Builtin
import Type.Util
import Type.State
import Type.Class
import Type.Plate.Trans		
import Util
import DDC.Main.Error
import Type.Pretty		()
import qualified Data.Set	as Set

-----
stage	= "Type.Plug"

-- | For classIds in this type that are not present in the environment set,
--	replace them with their corresponding variables.
--	This is part of generalisation. 
--	We'll add forall quantifier for these variables at a later stage.
--
plugClassIds 
	:: Set ClassId 		-- ^ ClassIds not to plug
	-> Type 		-- ^ The type to plug
	-> SquidM Type		-- ^ plugged type

plugClassIds env xx
	= transZM (transTableId 
		 	{ transV	= sinkVar
			, transT_leave	= plugT env })
	$ xx

plugT env t
 = case t of
	TClass k cid
	 | Set.member cid env	
	 -> 	return t

	 | otherwise
	 -> do	var	<- makeClassName cid
		Just c	<- lookupClass cid
	 	return	$ TVar (classKind c) var
		
	_ -> 	return t
	

-- | Get the set of region classes which are non-generalisable because
--   they are material in this type. 
staticRsDataT :: Type -> Set Type
staticRsDataT tt
 = case tt of
	TVar k v		
	 | k == kRegion		-> Set.singleton tt
	 | otherwise		-> Set.empty

	TClass k cid		
	 | k == kRegion		-> Set.singleton tt
	 | otherwise		-> Set.empty

	TSum k ts
	 | k == kEffect		-> Set.empty
	 | k == kClosure	-> Set.unions $ map staticRsDataT ts

	-- TODO: we're taking all args to be material, 
	--	which is a safe overapproximation. 
	TApp{}
	 -> case takeTData tt of
	 	Just (k, v, ts)	-> Set.unions $ map staticRsDataT ts
		_		-> Set.empty

	TFetters t fs		-> staticRsDataT t
	TConstrain t crs	-> staticRsDataT t
	
	TForall b k t		-> staticRsDataT t
	
	TFree v t		-> staticRsDataT t
	TDanger t1 t2		-> Set.unions $ map staticRsDataT [t1, t2]
	
	TError{}		-> Set.empty
	
	TCon{}			-> Set.empty
	TBot{}			-> Set.empty

	-- for data containing function objects
	TEffect{}		-> Set.empty
	
	_ 	-> panic stage
		$ "staticRsDataT: " ++ show tt
		

-- Region cids that are free in the closure of the outer-most function
--	constructor(s) are being shared with the caller. These functions
--	did not allocate those regions, so they be can't generalised here.
staticRsClosureT
	:: Type -> Set Type

staticRsClosureT t
 = case t of
	TFetters t fs		-> staticRsClosureT t
	TConstrain t crs	-> staticRsClosureT t

	TApp{} 
	 | Just (t1, t2, eff, clo)	<- takeTFun t
	 -> staticRsDataT clo

	-- TODO: we're taking all args to be material, 
	--	which is a safe over approximation.
	 | Just (v, k, ts)		<- takeTData t
	 -> Set.unions $ map staticRsClosureT ts

	_ 	-> Set.empty
