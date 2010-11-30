{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Constraint.Simplify.Reduce
	(reduce)
where
import DDC.Constraint.Simplify.Usage
import DDC.Constraint.Simplify.Collect
import DDC.Constraint.Util
import DDC.Constraint.Exp
import DDC.Solve.Location
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Type
import DDC.Constraint.Pretty		()
import Data.Sequence			(Seq)
import qualified Data.Sequence		as Seq
import qualified Data.Foldable		as Seq
import qualified Data.Map		as Map
-- import qualified Debug.Trace
import Control.Monad
import Util

stage		= "DDC.Constraint.Simplify"
-- debug		= True
-- trace ss x	= if debug then Debug.Trace.trace (pprStrPlain ss) x else x

-- | The reduce phase does the actual inlining and simplification.
reduce 	:: UseMap		-- ^ map of how vars are used.
	-> Table		-- ^ table of things to inline
	-> CTree
	-> CTree

reduce usage table tree
	= makeCBranch BNothing
	$ reduce1 usage table tree

-- make this advance through a cons-list dropping out unwanted constraints.
-- use IORef to do the substitutions, then eat up IORefs in a separate pass.
--- should be easy to make the second traversal be sequential, deepseqing the constraint list.


-- | Reduce a single constraint
reduce1 :: UseMap		-- ^ map of how vars are used.
	-> Table		-- ^ table of things to inline.
	-> CTree
	-> Seq CTree

reduce1 usage table cc
 = let	subEq	= subTT_noLoops (tableEq table)
	subEqV v
	 = case Map.lookup (TVar kValue (UVar v)) (tableEq table) of
		Just (TVar _ (UVar v'))	-> v'
		_			-> v

	subEqBind bb	
	 = case bb of
		BNothing	-> BNothing
		BLetGroup vs	-> BLetGroup 	$ map subEqV vs
		BLet    vs	-> BLet		$ map subEqV vs
		BLambda vs	-> BLambda	$ map subEqV vs
		BDecon  vs	-> BDecon	$ map subEqV vs

   in case cc of
	CBranch bind subs
	 -> Seq.singleton 
	  $ makeCBranch (subEqBind bind)
	  $ reorder 
	  $ join
	  $ fmap (reduce1 usage table) subs

	-- Eq ---------------------------------------------
	-- Ditch trivial eq constraints
	CEq _ (TVar _ v1) (TVar _ v2)
	 | v1 == v2
	 -> Seq.empty

	-- Ditch v1=v2 constraints when either of the vars are only used once.
	CEq _ t1@TVar{} t2@TVar{}
	 |   usedJustOnceInEq usage t1 
	  || usedJustOnceInEq usage t2
	 -> Seq.empty

	-- If we've substituted into an outermost variable we may have ended
	-- up with a boring v1 = v1 constraint, so ditch that out early.
	CEq src t1 t2				
	 -> mkCEq1 src (subEq t1) (subEq t2)

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


mkCEq1 :: TypeSource -> Type -> Type -> Seq CTree
mkCEq1 _ (TVar _ v1) (TVar _ v2)
	| v1 == v2	= Seq.empty
	
mkCEq1 src t1 t2	= Seq.singleton $ CEq src t1 t2


-- Reorder ----------------------------------------------------------------------------------------
-- | Reorder constraints into a standard ordering.
--   This only reorders constraints within the block.
--   TODO: Putting all the INST constraints last might improve inference for projections,
--         but I'm yet to find a concrete example.
reorder	:: Seq CTree -> Seq CTree
reorder cs
 = let	([eqs, mores, gens], others)
		= partitionBy [(=@=) CEq{}, (=@=) CMore{}, (=@=) CGen{}]
		$ Seq.toList cs
		
   in	join	$ Seq.fromList
		[ Seq.fromList eqs
 		, Seq.fromList mores
		, Seq.fromList others
		, Seq.fromList gens ]
