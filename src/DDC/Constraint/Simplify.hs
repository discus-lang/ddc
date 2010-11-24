{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Simplification of type constraints prior to solving.
--	The constraints returned from slurping the desugared code contain
--	a lot of intermediate bindings where the names aren't actually
--	needed by the Desugar -> Core transform. This is especially the case
--	with effect and closure information.
--
--	Simplifying the constraints here prior to solving keeps the number
--	of nodes in the graph down and makes life easier for Type.Util.Pack.
--
--	NOTE:	Now that we're using :> constraints for effects and closures
--		this simplifier isn't actually doing anything.
--
--	TODO: 	Not sure if we should really do it anyway.
--		If we had a more efficient graph representation it would be better
--		to store individual type constructors in nodes instead of compound types.
--
--	TODO:	At the least we should make the simplifier optional so it's
--		easier to debug the compiler with and without.
--
--	TODO:	Maybe we should be doing simplification of value type constraints as well,
--		though doing this may adversely affect error reporting because we attach
--		source positions to constraints instead of to type constructors.
-- 
module DDC.Constraint.Simplify
	(simplify)
where
--import qualified DDC.Constraint.Trans	as Trans
import DDC.Constraint.Exp
import DDC.Var
import Data.Sequence			as Seq
{-
import DDC.Type
import DDC.Util.FreeVars
import DDC.Main.Error
import qualified Shared.VarUtil		as Var
import qualified Data.Map		as Map
-}
import Util

-- stage = "DDC.Constraint.Simplify"

-- | Simplify some type constraints.
--	This only changes the effect and closure constraints, and does some simple
--	substitution which makes the constraint files easier to read in debug logs.
--	It also reduces the number of classes in the graph.
--	Unification and real solving is done by the constraint solver proper.
--
simplify 
	:: Set Var		-- ^ don't inline these vars. 
				--	These are the ones needed by the Desugar -> Core transform.
	-> Seq CTree		-- ^ constraints to simplify
	-> Seq CTree		-- ^ simplified constraints
	
simplify _ tree
 = tree
{-
 	-- collect up bound vars that can't be inlined because they appear
	-- in the body of value type consraints.
	vsInTypes	= collectNoInline tree

	-- don't inline constraints for vars that are needed by the desugar
	-- to core transform, or are present in value type constraints.
	vsNoInline	= Set.union vsNeeded vsInTypes
	
	tree'		= evalState (inline tree)
				tableZero { tableNoInline = vsNoInline }
	
   in {- trace 	( "simplify:\n"
		% "  vsNeeded    = " % vsNeeded		% "\n"
   		% "  vsInTypes   = " % vsInTypes 	% "\n") -}
		tree'

-- Inlining ---------------------------------------------------------------------------------------
-- | State of the constraint inliner.
data Table
	= Table
	{ tableNoInline		:: Set Var
	, tableSub		:: Map Var Type }

-- | Initial state of the constraint inliner.
tableZero
	= Table
	{ tableNoInline		= Set.empty
	, tableSub		= Map.empty }

-- | The inliner state monad.
type InlineM = State Table

-- Walk backwards over the type constraints.
--	We walk backwards because we don't want to risk adding information
--	to the graph /after/ generalisations are performed.
--
--	If an effect or closure constraint isn't marked as no-inlined then store
--	it in the map. If it *is* marked as noinline then substitute into it 
--	from the information in the map.

-- | Inline simple looking constraints in this constraint tree that aren't bound
--	by variables in the no-inline set held in the state monad.
--
inline :: [CTree] -> InlineM [CTree]
inline tree
 = do	tree1	<- inlineCollect [] tree
 	inlineDump [] tree1

-- | Collect up some constraints to inline, 
inlineCollect :: [CTree] -> [CTree] -> InlineM [CTree]
inlineCollect acc []
	= return $ reverse acc
	
inlineCollect acc (c:cs)
 = case c of
 	CBranch bind cc
	 -> do	cc'	<- inlineCollect [] cc
		inlineCollect (CBranch bind cc' : acc) cs
		
	CEq _ (TVar _ (UVar v1)) t2
	 -> do	noInline	<- gets tableNoInline
		
		let res
			-- can't inline this constraint, leave it alone
		 	| Set.member v1 noInline
			= inlineCollect (c : acc) cs

			-- leave value type constraints alone
			| varNameSpace v1 == NameType
			= inlineCollect (c : acc) cs

			-- we can inline this constraint, so slurp it into the map
			| otherwise
			= do	modify $ \s -> s { tableSub = Map.insert v1 t2 (tableSub s) }
				inlineCollect acc cs
				
		res
		
	-- leave other constraints alone
	_ ->	inlineCollect (c : acc) cs


-- | Do the actual inlining.
inlineDump 
	:: [CTree] 		-- ^ acc
	-> [CTree] 		-- ^ constraints to inline into.
	-> InlineM [CTree]	-- ^ final constraints.

inlineDump acc []
	= return $ reverse acc
	
inlineDump acc (c : cs)
 = case c of
 	CBranch bind cc
	 -> do	cc'	<- inlineDump [] cc
	 	inlineDump (CBranch bind cc' : acc) cs
		
	CEq sp t1@TVar{} t2
	 -> do	sub	<- gets tableSub
		let t2'	=  packT $ subFollowVT sub t2
		inlineDump (CEq sp t1 t2' : acc) cs

	CMore sp t1@TVar{} t2
	 -> do	sub	<- gets tableSub
		let t2'	=  packT $ subFollowVT sub t2
		inlineDump (CMore sp t1 t2' : acc) cs
		
	_ ->	inlineDump (c : acc) cs
		
		

-- | Substitute into this type
--	Substitute types for vars into this type.
--	TODO: Doesn't this just replicate stuff in the Type modules?

subFollowVT :: Map Var Type -> Type -> Type
subFollowVT sub tt
	= subFollowVT' sub Set.empty tt

subFollowVT' sub block tt
 = let	down	= subFollowVT' 	sub block
	downF	= subFollowVT_f sub block
   in case tt of
 	TForall  b k t		-> TForall b k (down t)

	TConstrain t crs	
	 -> makeTConstrain (down t )
		(Constraints	(Map.map down $ crsEq crs)
			  	(Map.map down $ crsMore crs)
				(map downF $ crsOther crs))

	TSum 	k ts		-> TSum	k (map down ts)
	TApp	t1 t2		-> TApp	(down t1) (down t2)
	TCon{}			-> tt
	TVar	_ (UVar v)
	 | Set.member v block	-> tt
	 | otherwise
	 -> case Map.lookup v sub of
	 	Nothing		-> tt
		Just t'		-> subFollowVT' sub (Set.insert v block) t'

	_ -> panic stage $ "subFollow: no match"

subFollowVT_f sub block ff
 = let down	= subFollowVT' sub block
   in case ff of
 	FConstraint v ts	-> FConstraint v (map down ts)
	FWhere t1 t2		-> FWhere t1 (down t2)
	FMore t1 t2		-> FMore t1 (down t2)
	FProj j v t1 t2		-> FProj j v (down t1) (down t2)


-- | Collect up effect and closure vars in these constraints that must
--	not be inlined because they appear free in a value type constraint.
--
--  Can't inline !e1 or $c1 if it appears like
--	t102 = t101 -(!e1 $c1)> t102
--
--	t102 might be unified with another type, which causes !e1 and $c1 
--	to also be unified. If we were to inline a binding for !e1 or $c1 
--	we would loose the name at this point and be unable to do the 
--	unification (== summing for effects and closures).
--
collectNoInline 
	:: [CTree] -> Set Var
	
collectNoInline bb
	= execState 
		(Trans.transM 
			Trans.Table { Trans.tableTransT = collectNoInlineT } 
			bb) 
		Set.empty

collectNoInlineT :: Type -> State (Set Var) Type
collectNoInlineT tt
	| isValueTypeFromVar tt
	= collectNoInlineT' tt
	
	| otherwise
	= return tt
	
collectNoInlineT' tt
 = do	let free	= freeVars tt

	-- we're only interested in effect and closure vars here
	let keepV v
		| not $ Var.isCtorName v
		,    varNameSpace v == NameEffect
		  || varNameSpace v == NameClosure	
		= True
	 
		| otherwise
		= False

	let free_keep	= Set.filter keepV free	 

	modify $ \s -> Set.union s free_keep
	return tt 	


-- | Quick check if this type has a value kind, used when deciding what
--   type constraints to inline.
isValueTypeFromVar tt
 = case tt of
	TVar _ (UVar v)
	 | varNameSpace v == NameType
	 	-> True
		
	_	-> False

-}