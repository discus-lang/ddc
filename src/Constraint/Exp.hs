
-- | The tree of type constraints.
module	Constraint.Exp
	( CTree(..)
	, CBind(..) )
where
import Type.Exp
import Type.Location
import DDC.Var
import Data.Map			(Map)


-- CTree ------------------------------------------------------------------------------------------
-- | The tree of type constraints.
--	In most of these constraints, the first Type parameter should always be 
--	a TClass or a TVar.
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


	-- | A complete type definition for a variable. 
	--	Imported from a module, or via the FFI. Assumed to be correct.
	| CDef		TypeSource Type Type	

	-- | A type signature from the source program.
	--	These can contain partial information about the type of bound variable.
	| CSig		TypeSource Type Type	

	-- | A type equality constraint.
	| CEq		TypeSource Type Type

	-- | Some type equalities. 
	--	All the types in the list are to be taken as equal, which saves having to write
	--	a large number of CEq constraints. The first one should be a TVar
	| CEqs		TypeSource [Type]

	-- | A type-class constraint.
	| CClass	TypeSource Var [Type]

	-- | A projection constraint.
	| CProject	TypeSource 	--  source of the constraint.
			TProj		--  the sort of projection.
			Var 		--  type variable to tie to the projection function.
			Type 		--  the type that guides what projection dictionary to use,
					--	that is, the type of the object being projection.
			Type		--  type to unify the type of the instance function once 
			 		--	it has been determined.
			
	-- | An instantiate of a type scheme. The solver will have to wait until the scheme
	--	is available before it can resolve this projection.
	| CInst		TypeSource 
				Var 	--  type var to equate with the instantiated type.
				Var	--  type var of the scheme to instantiate.

	-- | Generalise a type scheme.
	--	When we hit this one we know that all the constraints from the bound 
	--	variable have been added to the graph and that it's now safe to generalise
	--	its type.
	| CGen		TypeSource Type


	-- dictionaries ---------------
	-- | Carries data field definitions.
	--	One of these is generated for each data def in the source.
	| CDataFields	
		TypeSource 		--  source position
		Var 			--  var of data constructor
		[Var] 			--  parameter vars of constructor.
		[(Var, Type)]		--  (field name, field type)

	-- | Carries a projection dictionary.
	| CDictProject	
		TypeSource 		--  source position
		Type 			--  type of projection
		(Map Var Var)		--  map of field label to name of instance function.

	-- | Says that a type class has a certain instance.
	| CClassInst	
		TypeSource		--  source position
		Var 			--  var of type class
		[Type]			--  type parameters of class

	-- | Gathers up effect and closure visible at top level.
	--	ie, all effects caused by cafs, and all external vars used by the module.
	--	There should be one of each of these in the constraint list generated
	--	by Desugar.Slurp
	--
	--	TODO: Is this still being used?
	--
	| CTopEffect	Type
	| CTopClosure	Type


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
	= BNil		
	
	-- | Delimits the scope of a group of mutually recursive let bindings
	| BLetGroup	[Var]
	
	-- | Some let-bound variables.
	| BLet		[Var]
	
	-- | Some lambda-bound variables.
	| BLambda	[Var]
	
	-- | Some match (deconstructor) bound variables.
	| BDecon	[Var]
	deriving (Show, Eq, Ord)
	
	