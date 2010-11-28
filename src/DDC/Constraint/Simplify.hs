{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Simplification of type constraints prior to solving.
--	The constraints returned from slurping the desugared code contain a lot of intermediate
--	bindings where the names aren't actually needed by the Desugar -> Core transform.
--	This is especially the case with effect and closure information. All of the variables
--	present in the constraints end up in the type graph, so it's better to eliminate them
--	during a pre-solver phase before loading them into the graph.
--
-- 	We must keep bindings for wanted variables because the Desugar -> Core transform needs them.
-- 
module DDC.Constraint.Simplify
	(simplify)
where
import DDC.Constraint.Simplify.Reduce
import DDC.Constraint.Simplify.Collect
import DDC.Constraint.Exp
import DDC.Var
import DDC.Constraint.Pretty		()
import Data.Sequence			(Seq)
import Util



-- | Simplify some type constraints.
simplify 
	:: Set Var		-- ^ Wanted type vars that we must preserve, don't eliminate them.
	-> Seq CTree		-- ^ Constraints to simplify
	-> Seq CTree		-- ^ Simplified constraints
	
simplify wanted tree
 = let	table	= collect wanted (CBranch BNothing tree)
   in	reduce wanted table tree




