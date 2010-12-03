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
-- import qualified Debug.Trace
import Util

stage		= "DDC.Constraint.Simplify"
-- debug		= True
-- trace ss x	= if debug then Debug.Trace.trace (pprStrPlain ss) x else x

-- | The reduce phase does the actual inlining and simplification.
reduce 	:: UseMap		-- ^ map of how vars are used.
	-> Table		-- ^ table of things to inline
	-> CTree
	-> IO CTree

reduce usage table tree
 = do	ls	<- reduce1 usage table tree
	return	$ makeCBranch BNothing $ maybeToList ls

-- | Reduce a single constraint
reduce1 :: UseMap		-- ^ map of how vars are used.
	-> Table		-- ^ table of things to inline.
	-> CTree
	-> IO (Maybe CTree)

reduce1 usage table cc
 = let	subEq t  	= return t	-- = subTT_noLoops (tableEq table)
--	subEqV v 	= return v
	
	subEqBind bb	= return bb
{-
	 = case bb of
		BNothing	-> BNothing
		BLetGroup vs	-> BLetGroup 	$ map subEqV vs
		BLet    vs	-> BLet		$ map subEqV vs
		BLambda vs	-> BLambda	$ map subEqV vs
		BDecon  vs	-> BDecon	$ map subEqV vs
-}
   in case cc of
	CBranch bind subs
	 -> do	ls	<- mapM (reduce1 usage table) subs
		bind'	<- subEqBind bind
		return	$ Just 
			$ makeCBranch bind'
			$ reorder
			$ catMaybes ls

	-- Eq ---------------------------------------------
	-- Ditch trivial eq constraints
	CEq _ (TVar _ v1) (TVar _ v2)
	 | v1 == v2
	 -> return Nothing

	-- Ditch v1=v2 constraints when either of the vars are only used once.
	CEq _ t1@TVar{} t2@TVar{}
	 |   usedJustOnceInEq usage t1 
	  || usedJustOnceInEq usage t2
	 -> return Nothing

	-- If we've substituted into an outermost variable we may have ended
	-- up with a boring v1 = v1 constraint, so ditch that out early.
	CEq src t1 t2				
	 -> do	t1'	<- subEq t1
		t2'	<- subEq t2
		return	$ mkCEq1 src t1' t2'

	-- More -------------------------------------------
	-- Ditch  :> 0 constrints
	CMore _ _ (TSum _ [])	
	 -> return Nothing

	-- Ditch  :> constraints that we're inlining as Eqs.
{-	CMore _ t1 _
	 | Map.member t1 (tableEq table)
	 -> return Nothing
-}
	CMore src t1 t2		
	 -> do	t2'	<- subEq t2
		return $ Just $ CMore src t1 t2'


	-- Project ----------------------------------------
	CProject src j v t1 t2	
	 -> do	t1'	<- subEq t1
		t2'	<- subEq t2
		return $ Just $ CProject src j v t1' t2'


	-- Gen -------------------------------------------
	CInst{}	-> return $ Just cc
	CGen{}	-> return $ Just cc
		
	_	-> panic stage $ "reduce1: no match for" %% cc


mkCEq1 :: TypeSource -> Type -> Type -> Maybe CTree
mkCEq1 _ (TVar _ v1) (TVar _ v2)
	| v1 == v2	= Nothing
	
mkCEq1 src t1 t2	= Just $ CEq src t1 t2


-- Reorder ----------------------------------------------------------------------------------------
-- | Reorder constraints into a standard ordering.
--   This only reorders constraints within the block.
--   TODO: Putting all the INST constraints last might improve inference for projections,
--         but I'm yet to find a concrete example.
reorder	:: [CTree] -> [CTree]
reorder cs
 = let	([eqs, mores, gens], others)
		= partitionBy [isCEq, isCMore, isCGen] cs
		
   in	concat	[ eqs
 		, mores
		, others
		, gens ]
