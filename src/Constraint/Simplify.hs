
-- Simplification of type constraints prior to solving.
--	The constraints returned from slurping the desugared code contain
--	a lot of intermediate bindings where the binding name isn't actually
--	needed by the Desugar -> Core transform. This is especially the case
--	with effect and closure information.
--
--	Simplifying the constraints here prior to solving keeps the number
--	of nodes in the graph down and makes life easier for Type.Util.Pack.
--
module Constraint.Simplify
	(simplify)
	
where

----

import Constraint.Exp
import Type.Exp

import Util

import Data.Map			as Map
import qualified Data.Map	(Map)

import Data.Set			as Set
import qualified Data.Set	(Set)

-----

simplify 
	:: Set Var		-- ^ don't inline these vars
				--	these are the vars needed by the Desugar -> Core transform.
	-> CTree		-- ^ constraints to simplify
	-> CTree		-- ^ simplified constraints
	
simplify vsNoInline tree
 = tree	























