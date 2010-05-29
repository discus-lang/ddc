
-- | Type Expressions
module Type.Exp
	( module DDC.Type.ClassId
	, module DDC.Type.KiCon
	, module DDC.Type.TyCon
	, Super		(..)
	, Kind		(..)
	, Type		(..)
	, Bind		(..)
	, Constraints	(..)
	, TProj		(..)
	, Fetter  	(..)
	, TypeError	(..)

	, Data
	, Region
	, Effect
	, Closure
	, Witness)
where
import Util
import DDC.Type.ClassId
import DDC.Type.KiCon
import DDC.Type.TyCon
import DDC.Main.Error
import DDC.Var


-- Superkinds -------------------------------------------------------------------------------------
data Super
	-- | (+) Prop. The superkind of witness types.
	--   eg: MkConst %r1 :: Const %r1 :: +
	= SProp			

	-- | ([]) Box. The superkind of non-witness types.
	--   eg: 5 :: Int :: * :: []
	| SBox			
	
	-- | Superkind functions.
	| SFun	Kind	Super	
	deriving (Show, Eq)


-- Kinds ------------------------------------------------------------------------------------------
data Kind
	-- | Missing or as-yet-unknown kind information.
	= KNil

	-- | A kind constructor.
	| KCon	KiCon	Super

	-- | Dependent kind abstraction. Equivalent to (PI (a : k1). k2).
	--   We use the de Bruijn representation for these, so the body uses TIndex to 
	--   refer to the parameter type, instead of named variables.
	| KFun	Kind	Kind

	-- | Dependent kind application.
	| KApp	Kind	Type

	-- | Sum of witness kinds. This always has superkind (+).
	--   Used for joining witness kinds, like +{ Const %r1; DeepConst a }
	| KSum	[Kind]
	deriving (Show, Eq)	


-- Types ------------------------------------------------------------------------------------------
-- Synonyms to help with documentation.
type Data	= Type
type Region	= Type
type Effect	= Type
type Closure	= Type
type Witness	= Type

-- | Each of the constructors has enough information so that its kind can be 
--   determined efficiently. For some this means putting the kind directly
--   in the constructor.
data Type	
	-- | Missing or as-yet-unknown type information. 
	= TNil

	-- | A type variable.
	| TVar     	Kind 	Var

	-- | A type constructor.
	| TCon		TyCon

	-- | A type summation \/ least upper bound.
	--   Used for joining effect, closure, and witness types.
	--   If there are no elements in the list this means "bottom"
	| TSum		Kind 	[Type]

	-- | Type application.
	| TApp		Type	Type

	-- | Quantification.
	| TForall	Bind 	Kind	Type

	-- Constrained types.
	-- TODO: We are currently moving the representation from TFetters to TConstrain.
	-- TConstrain uses finite maps, not unsorted lists, so is much more efficient.
	| TFetters	Type	[Fetter]	-- ^ Holds extra constraint information.
	| TConstrain	Type	Constraints	-- ^ Holds extra constraint information.
			

	-- Special Purpose Constructors -------------------

	-- | Used in the kinds of witness constructors only.
	--   A de Bruijn index.
	| TIndex	Kind	Int
	
	-- | Used in core types only. 
	--   A type variable with an embedded :> constraint.
	| TVarMore	Kind 	Var	Type
		
	-- | Used in the solver only.
	--   A reference to some equivalence class in the type graph.
	--   Also known as a "meta" type variable.
	| TClass   	Kind 	ClassId

	-- | Used in the solver only.
	--   Represents an error in the type.
	| TError	Kind	TypeError
	deriving (Show, Eq)


-- | A binder.
data Bind
	-- | No binding.
	= BNil

	-- | Unbounded quantification.
	| BVar	Var
	
	-- | Bounded quantification. Type of bound variable is :> t2.
	| BMore	Var Type
	deriving (Show, Eq)


-- | A bound occurrence of a variable.
--   TODO: Use this to merge TVar, TVarMore, TIndex and TClass into TVar.
data Bound
	= UVar	 Var
	| UMore	 Var Type
	| UIndex Int
	| UClass ClassId
	deriving (Show, Eq)


-- | Stores information about a type error directy in a type.
--	Used in the TError constructor of Type.
--	Used during type inference only.
data TypeError
	= TypeError			-- ^ types that couldn't be unified, or some other problem.
	| TypeErrorLoop	 Type Type	-- ^ a recursive type equation 
					--	(mu t1. t2), 	where t1 can appear in t2.
					--			t1 is a TClass or a TVar.
	deriving (Show, Eq)


instance Ord Type where
 compare t1 t2
	| TClass _ a	<- t1
	, TClass _ b	<- t2
	= compare a b
	
	| Just v1	<- takeVar t1
	, Just v2	<- takeVar t2
	= compare v1 v2
	
	| Just v1	<- takeVar t1
	, TClass{}	<- t2
	= GT
	
	| TClass{}	<- t1
	, Just v2	<- takeVar t2
	= LT

	| otherwise
 	= panic "Type.Exp" 
	$ "compare: can't compare type for ordering\n"
	% "    t1 = " % show t1	% "\n"
	% "    t2 = " % show t2 % "\n"


takeVar tt
 = case tt of
	TVar _ v	-> Just v
	TVarMore _ v _	-> Just v
	_		-> Nothing


-- Constraints ------------------------------------------------------------------------------------

-- | Constraints that can be applied to a type.
data Constraints
	= Constraints 
	{ crsEq		:: Map Type Type
	, crsMore	:: Map Type Type
	, crsOther	:: [Fetter] }
	deriving (Show, Eq)


-- | A Fetter is a piece of type information which isn't part of the type's shape.
--   TODO: Refactor to only contain FConstraint and FProj. The others are in the Constraints type.
--         Do we really want to keep FWhere and FMore for the solver? 
--         If so use Bind for the lhs and merge them together, also for TypeError above.
data Fetter
	= FConstraint	Var	[Type]		-- ^ Constraint between types.
	| FWhere	Type	Type		-- ^ Equality of types, t1 must be TVar or TClass
	| FMore		Type	Type		-- ^ t1 :> t2

	-- | projections
	--   TODO: refactor this into a special constructor, and make FConstraint
	--	   above take that constructor instead of a plain var.
	| FProj		TProj	
			Var 			-- var to tie the instantiated projection function to.
			Type 			-- type of the dictionary to choose the projection from.
			Type 			-- type to unify the projection function with, once it's resolved.
				
	deriving (Show, Eq, Ord)


-- | Represents field and field reference projections.
--   TODO: Check is this only used in the solver?
data TProj
	= TJField  !Var				-- ^ A field projection.   		(.fieldLabel)
	| TJFieldR !Var				-- ^ A field reference projection.	(#fieldLabel)

	| TJIndex  !Var				-- ^ Indexed field projection		(.<int>)
	| TJIndexR !Var				-- ^ Indexed field reference projection	(#<int>)
	deriving (Show, Eq, Ord)

