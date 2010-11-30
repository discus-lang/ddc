{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- | The tree of type constraints.
module	DDC.Constraint.Exp
	( CTree(..)
	, CBind(..) )
where
import DDC.Solve.Location
import DDC.Type.Exp
import DDC.Var

-- | The tree of type constraints.
--   In most of these constraints, the first Type parameter should always be 
--   a TClass or a TVar.
--   TODO: Change representation to enforce this. Maybe use Bind.
--
--   TODO: Shift the internal constraints used internally by the solver somewhere else.
--	   We also want to break the dictionaries out into a different type.
--
data	CTree
	= -- | An empty tree, used as a place holder.
	  CTreeNil				

	-- | A branch representing the constraints from an source level expression. 
	--	If the expression binds variables, as in a let or lambda-expression,
	--	then these will be present in the branchBind field.
	| CBranch  
	  { -- | vars bound by this branch.
	    branchBind	:: CBind		

	    -- | sub constraints
	  , branchSub	:: [CTree] }		

	-- | A type equality constraint.
	| CEq		TypeSource Type Type

	-- | Type inequality (t1 :> t2)
	| CMore		TypeSource Type Type

	-- | A projection constraint.
	| CProject	TypeSource 	--  source of the constraint.
			TProj		--  the sort of projection.
			Var 		--  type variable to tie to the projection function.
			Type 		--  the type that guides what projection dictionary to use,
					--	that is, the type of the object being projection.
			Type		--  type to unify the type of the instance function once 
			 		--	it has been determined.
			
	-- | Instantiate a type scheme. 
	--   The solver must wait until the scheme is available before it can can instantiate it.
	| CInst		TypeSource 
				Var 	--  type var to equate with the instantiated type.
				Var	--  type var of the scheme to instantiate.

	-- | Generalise a type scheme.
	--	When we hit this one we know that all the constraints from the bound 
	--	variable have been added to the graph and that it's now safe to generalise
	--	its type.
	| CGen		TypeSource Type


	--------------
	-- These constraints are used internally by the solver.
	--	Support for type based projections means we can't determine a call graph before we start
	--	the solver. These ctors are used to help with reordering constraints on the fly.

	-- (used internaly to solver).
	--	A marker to remind the solver to leave a branch because all the constraints from
	--	it have now been added to the graph.
	| CLeave	CBind

	-- (used internally to solver).
	--	A marker that triggers a grind (reduction) of the graph.
	| CGrind

	-- (used internally to solver).
	--	A marker to remind us to instantiate a lambda-bound variabe.
	| CInstLambda		TypeSource Var Var

	-- (used internally to solver).
	--	A marker to remind us to instantiate a let-bound variabe.
	| CInstLet		TypeSource Var Var

	-- (used internally to solver).
	--	A marker to remind us to instantiate a letrec-bound variabe.
	| CInstLetRec		TypeSource Var Var
	deriving (Show)


-- CBind ------------------------------------------------------------------------------------------
-- | Represents bound varaibles, and how they were bound.
--	These are attached to CBranch nodes of the constraint tree.
--
data	CBind
	-- nothing is bound here
	= BNothing
	
	-- | Delimits the scope of a group of mutually recursive let bindings
	| BLetGroup	[Var]
	
	-- | Some let-bound variables.
	| BLet		[Var]
	
	-- | Some lambda-bound variables.
	| BLambda	[Var]
	
	-- | Some match (deconstructor) bound variables.
	| BDecon	[Var]
	deriving (Show, Eq, Ord)
	
	