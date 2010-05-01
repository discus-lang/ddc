
module Type.Check.GraphicalData
	(checkGraphicalDataT)
where
import Type.Exp
import Type.Builtin
import Type.Util
import Type.Plate.Collect
import Util.Graph.Deps		(graphReachable1_nr)
import qualified Data.Set	as Set
import qualified Data.Map	as Map


-- | Check whether this type is graphical in its data portion
--	and hence would be inifinite if we tried to construct it into the flat form.
--
--	If it is graphical, returns the list of cids which are on a loop.
--
checkGraphicalDataT :: Type -> [ClassId]

checkGraphicalDataT (TFetters t fs)
 = let	
	-- select only the data portion of this type's FLets
 	fsData		= [ f 	| f@(FWhere t1 t2) <- fs
	 			, kindOfType t1 == Just kValue]

	-- these are the data cids that we have FLets for
	cidsData	= map (\(FWhere (TClass k cid) t2) -> cid) fsData
	cidsDataS	= Set.fromList cidsData

	-- build a map of what cids are reachable from what lets in a single step
	ccData		= Map.fromList

			$ map (\(FWhere t1@(TClass k cid) t2)
				-> ( cid
				   , Set.fromList 
				   	$ filter (\c -> Set.member c cidsDataS) 
					$ Set.toList
					$ collectClassIds t2))

			$ fsData

	-- expand the reachability map to the list of all the cids reachable in however many steps
	reachable	= map (\cid -> (cid, graphReachable1_nr ccData cid))
			$ cidsData

	-- return the list of all the cids which can reach themselves, so are part of a loop.
	loopCids	= [cid	| (cid, reach) <- reachable
				, Set.member cid reach ]
				
  in	loopCids
	
checkGraphicalDataT t
 = []
