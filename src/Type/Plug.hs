
-- | Replace classIds with their corresponding variables.
--	This is part of the process of type scheme generalisation.
module Type.Plug
	( plugClassIds
	, staticRsDataT
	, staticRsClosureT)	
where
import Type.State
import Type.Class
import Util
import DDC.Solve.Naming
import DDC.Type
import DDC.Type.Transform
import DDC.Main.Error
import qualified Data.Set	as Set

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
	TVar k (UClass cid)
	 | Set.member cid env	
	 -> 	return t

	 | otherwise
	 -> do	var	<- getCanonicalNameOfClass cid
		Just c	<- lookupClass cid
	 	return	$ TVar (classKind c) $ UVar var
		
	_ -> 	return t
	

-- | Get the set of region classes which are non-generalisable because
--   they are material in this type. 
staticRsDataT :: Type -> Set Type
staticRsDataT tt
 = case tt of
	TVar k v		
	 | k == kRegion		-> Set.singleton tt
	 | otherwise		-> Set.empty

	TSum k ts
	 | k == kEffect		-> Set.empty
	 | k == kClosure	-> Set.unions $ map staticRsDataT ts

	-- TODO: we're taking all args to be material, 
	--	which is a safe overapproximation. 
	TApp{}
	 | Just (k, v, ts)	<- takeTData tt
	 -> Set.unions $ map staticRsDataT ts
	 
	 | Just (v, t)		<- takeTFree tt
	 -> staticRsDataT t
	
	 | Just (t1, t2)	<- takeTDanger tt
	 -> Set.unions $ map staticRsDataT [t1, t2]
	
	 | otherwise		-> Set.empty

	TConstrain t crs	-> staticRsDataT t
	
	TForall b k t		-> staticRsDataT t	
	TError{}		-> Set.empty
	
	TCon{}			-> Set.empty
	
	_ 	-> panic stage
		$ "staticRsDataT: " ++ show tt
		

-- Region cids that are free in the closure of the outer-most function
--	constructor(s) are being shared with the caller. These functions
--	did not allocate those regions, so they be can't generalised here.
staticRsClosureT :: Type -> Set Type
staticRsClosureT tt
 = case tt of
	TConstrain t crs	-> staticRsClosureT t

	TApp{} 
	 | Just (t1, t2, eff, clo)	<- takeTFun tt
	 -> staticRsDataT clo

	 | Just (v, t2)			<- takeTFree tt
	 -> staticRsDataT t2
	
	 | Just (t1, t2)		<- takeTDanger tt
	 -> Set.unions $ map staticRsDataT [t1, t2]

	 -- TODO: we're taking all args to be material, 
	 --	which is a safe over approximation.
	 | Just (v, k, ts)		<- takeTData tt
	 -> Set.unions $ map staticRsClosureT ts

	TSum k ts
	 | k == kClosure
	 -> Set.unions $ map staticRsClosureT ts

	_ 	-> Set.empty
