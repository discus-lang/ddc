{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Simplification of type constraints prior to solving.
--	The constraints returned from slurping the desugared code contain a lot of intermediate
--	bindings where the names aren't actually needed by the Desugar -> Core transform.
--	This is especially the case with effect and closure information. All of the variables
--	present in the constraints end up in the type graph, so it's better to eliminate them
--	during a pre-solver phase before loading them into the graph.
--
--	In all cases the programs should compile with and without this simplifier, and you 
--	can disable it on the command line with the -debug-no-constraint-simplifier compiler flag.
--
-- 	We must keep bindings for wanted variables because the Desugar -> Core transform needs them.
-- 
module DDC.Constraint.Simplify
	(simplify)
where
import DDC.Constraint.Simplify.Usage
import DDC.Constraint.Simplify.Reduce
import DDC.Constraint.Simplify.Collect
import DDC.Constraint.Exp
import DDC.Type
import DDC.Var
import DDC.Constraint.Pretty		()
import qualified Data.Set		as Set
import Util

-- | Simplify some type constraints.
simplify 
	:: Set Var		-- ^ Wanted type vars that we must preserve, don't eliminate them.
	-> CTree		-- ^ Constraints to simplify
	-> IO (CTree, UseMap)	-- ^ Simplified constraints, and usage map for dumping.
	
simplify wanted tree
 = do	usage	<- emptyUsage

	mapM_ (addUsage usage UsedWanted)
		[ TVar kValue (UVar v)
		| v <- Set.toList wanted ]

	slurpUsage usage tree

	table	<- {-# SCC "Desugar/slurp/simplify/collect" #-}
		   collect usage tree

	tree'	<- {-# SCC "Desugar/slurp/simplify/reduce" #-}
		   reduce usage table tree

	return (tree', usage)




