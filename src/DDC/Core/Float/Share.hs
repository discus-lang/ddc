{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Core.Float.Share where
import DDC.Var
import qualified Data.Map	as Map
import Data.Map			(Map)

-- This is some very simple CSE ---------
-- We could do this with a more general mechanism, but these are important enough, and fast enough
--	to embed directly in the let-floater.

-- We don't merge this with the Table above, because that contains reconstructed
--	bottom-up information as well. This sharing is only top-down.

-- shared results
data Share
	= Share
	{ -- variable substition
	  shareVarSub		:: Map Var Var

	  -- Elimination of forcing is important, because most function args are in
	  --	unknown regions and must be forced before use, and tend to have
	  --	multiple bound occurances inside the function.
	, shareForcings		:: Map Var Var		-- v2 = force v1

	  -- Elimination of unboxings follows from elimination of forcings
	  -- 	v2 = unbox v1
	  --	v3 is the region that the unboxed data was in
	  --	We can move unboxings from mutable regions, so long as we don't carry the result over
	  --	a statement that writes to that region.
	, shareUnboxings	:: Map Var (Var, Var)
	}

-- empty share table
shareZero
	= Share
	{ shareVarSub		= Map.empty
	, shareForcings		= Map.empty 
	, shareUnboxings	= Map.empty }

-- sink a variable using the share table
sinkVar :: Share -> Var -> Var
sinkVar share v1
 = case Map.lookup v1 (shareVarSub share) of
 	Just v2	-> sinkVar share v2
	_	-> v1


