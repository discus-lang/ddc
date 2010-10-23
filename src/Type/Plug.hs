
-- | Replace classIds with their corresponding variables.
--	This is part of the process of type scheme generalisation.
module Type.Plug
	(plugClassIds)
where
import DDC.Solve.State
import DDC.Type
import DDC.Type.Transform
import Util
import qualified Data.Set	as Set

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
	

