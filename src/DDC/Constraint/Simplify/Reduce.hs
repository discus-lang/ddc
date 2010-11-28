{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Constraint.Simplify.Reduce
	(reduce)
where
import DDC.Constraint.Simplify.Collect
import DDC.Constraint.Exp
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Type
import DDC.Var
import DDC.Constraint.Pretty		()
import Data.Sequence			(Seq)
import qualified Data.Sequence		as Seq
import qualified Data.Foldable		as Seq
import qualified Data.Map		as Map
import Control.Monad
import Util

stage = "DDC.Constraint.Simplify"

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
	  $ cc { branchSub 	= reorder
				$ reduce wanted table 
					$ branchSub cc }

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


	-- Project ----------------------------------------
	CProject src j v t1 t2	
	 -> Seq.singleton
	 $  CProject src j v (subEq t1) (subEq t2)


	-- Gen -------------------------------------------
	CInst{}			-> Seq.singleton cc
	CGen{}			-> Seq.singleton cc
		
	_			-> panic stage $ "reduce1: no match for" %% cc


-- Reorder ----------------------------------------------------------------------------------------
-- | Reorder constraints into a standard ordering.
--   This only reorders constraints within the block.
--   TODO: Putting all the INST constraints last might improve inference for projections,
--         but I'm yet to find a concrete example.
reorder	:: Seq CTree -> Seq CTree
reorder cs
 = let	([eqs, mores, gens], others)
		= partitionFs
			[ (=@=) CEq{},  (=@=) CMore{},    (=@=) CGen{} ]
		$ Seq.toList cs
		
   in	join	$ Seq.fromList
		[ Seq.fromList eqs
 		, Seq.fromList mores
		, Seq.fromList others
		, Seq.fromList gens ]
