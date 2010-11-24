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
import DDC.Constraint.Simplify.Collect
import DDC.Constraint.Exp
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Type
import DDC.Var
import DDC.Constraint.Pretty		()
import Data.Set				(Set)
import Data.Sequence			(Seq)
import qualified Data.Sequence		as Seq
import qualified Data.Map		as Map
import Control.Monad

stage = "DDC.Constraint.Simplify"


-- | Simplify some type constraints.
simplify 
	:: Set Var		-- ^ Wanted type vars that we must preserve, don't eliminate them.
	-> Seq CTree		-- ^ Constraints to simplify
	-> Seq CTree		-- ^ Simplified constraints
	
simplify wanted tree
 = let	table	= collect wanted (CBranch BNothing tree)
   in	reduce wanted table tree


-- Reduce -----------------------------------------------------------------------------------------
-- | The reduce phase does the actual inlining and simplification.
reduce 	:: Set Var		-- ^ wanted vars
	-> Table		-- ^ table of things to inline
	-> Seq CTree
	-> Seq CTree

reduce wanted table cs
	= join $ fmap (reduce1 wanted table) cs


-- | Reduce a single constraint
reduce1 :: Set Var		-- ^ wanted vars.
	-> Table		-- ^ table of things to inline.
	-> CTree
	-> Seq CTree

reduce1 wanted table cc
 = let	subEq	= subTT_noLoops (tableEq table)
   in case cc of
	CBranch{}	
	 -> Seq.singleton 
	  $ cc { branchSub = reduce wanted table $ branchSub cc }

	-- Eq ---------------------------------------------
	-- Ditch eq constraints that are being inlined.
	CEq _ t1 _
	 |  Map.member t1 $ tableEq table
	 -> Seq.empty

	CEq src t1 t2				
	 -> Seq.singleton   $ CEq src t1 (subEq t2)

	-- Eqs --------------------------------------------
	-- Ditch single equalities.
	CEqs _ [_]
	 -> Seq.empty

	-- Ditch equalities that are being inlined
	CEqs _ 	[t1, TVar{}]
	 | Map.member t1 $ tableEq table	
	 -> Seq.empty

	CEqs src ts
	 -> Seq.singleton
	 $  CEqs src (map subEq ts)

	-- More -------------------------------------------
	-- Ditch  :> 0 constrints
	CMore _ _ (TSum _ [])	-> Seq.empty

	CMore src t1 t2		
	 -> Seq.singleton 
	  $ CMore src t1 (subEq t2)


	CInst{}			-> Seq.singleton cc
	CGen{}			-> Seq.singleton cc
	CProject{}		-> Seq.singleton cc
	
	-- TODO: these should really go into the solver Problem
	--	 instead of being their own constraints.
	CClassInst{}		-> Seq.singleton cc
	CDictProject{}		-> Seq.singleton cc
	
	_			-> panic stage $ "reduce1: no match for" %% cc

