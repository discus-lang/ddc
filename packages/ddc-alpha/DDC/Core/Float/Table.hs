{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Core.Float.Table where
import Core.BoundUse
import DDC.Core.Float.Stats
import DDC.Core.Exp
import DDC.Type
import DDC.Var
import Data.Map			(Map)
import Data.Set			(Set)
import qualified Data.Set	as Set
import qualified Data.Map	as Map

-- How many value lambdas we are in from the top level.
type Level	= Int

-- | inliner table
data Table
	= Table
	{ -- vars of bindings to not float
	  tableNoFloat	 	:: Set Var

	  -- how each bound varable is uses
	  --	(obtained via Core.BoundUse)
	, tableBoundUse		:: Map Var [Use]

	  -- vars of regions that are known to be constant
	, tableConstRegions	:: Set Var

	  -- vars of types that are known to be (deep) constant
	, tableConstTypes	:: Set Var

	  -- vars of effects that are known to be pure
	, tablePureEffects	:: Set Var

 	  -- bindings currently in motion along with the effect of that binding.
	, tableBinds		:: Map Var (Exp, Effect)


	  -- stats about what got moved / not moved
	, tableStats		:: Stats }
	


-- empty inliner table
tableZero
	= Table
	{ tableNoFloat		= Set.empty
	, tableBoundUse		= Map.empty
	, tableConstRegions	= Set.empty
	, tableConstTypes	= Set.empty
	, tablePureEffects	= Set.empty
	, tableBinds		= Map.empty 
	
	, tableStats		= statsZero }

-- horror
tableStats_  = ( tableStats, \x s -> s { tableStats = x })

