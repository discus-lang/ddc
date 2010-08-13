{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- | Type Expressions
module DDC.Type.Exp
	( module DDC.Type.Exp.ClassId
	, module DDC.Type.Exp.KiCon
	, module DDC.Type.Exp.TyCon
	, Super		(..)
	, Kind		(..)
	, Type		(..)
	, Bind		(..)
	, Bound		(..)
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
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Type.Exp.ClassId
import DDC.Type.Exp.KiCon
import DDC.Type.Exp.TyCon
import DDC.Var
import Data.Map		(Map)

-- Superkinds -------------------------------------------------------------------------------------
data Super
	-- | @(+)@ Prop. The superkind of witness types.
	--   eg: @MkConst %r1 :: Const %r1 :: +@
	= SProp			

	-- | @([])@ Box. The superkind of non-witness types.
	--   eg: @5 :: Int :: * :: []@
	| SBox			
	
	-- | Superkind functions. The left is always a (non-super) kind.
	| SFun	Kind	Super	
	deriving (Show, Eq)


-- Kinds ------------------------------------------------------------------------------------------
data Kind
	-- | Missing or as-yet-unknown kind information.
	= KNil

	-- | A kind constructor.
	| KCon	KiCon	Super

	-- | Dependent kind abstraction. Equivalent to @(PI (a : k1). k2)@.
	--   We use the de Bruijn representation for these, so the body uses `TIndex` to 
	--   refer to the parameter type, instead of named variables.
	| KFun	Kind	Kind

	-- | Dependent kind application.
	| KApp	Kind	Type

	-- | Sum of witness kinds. This always has superkind @(+)@.
	--   Used for joining witness kinds, like `+{ Const %r1; DeepConst a }`
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

	-- | A bound occurrence of a variable.
	| TVar     	Kind 	Bound

	-- | A type constructor.
	| TCon		TyCon

	-- | A type summation \/ least upper bound.
	--   Used for joining effect, closure, and witness types.
	--   If there are no elements in the list this means 'bottom'.
	| TSum		Kind 	[Type]

	-- | Type application.
	| TApp		Type	Type

	-- | Universal quantification.
	| TForall	Bind 	Kind	Type

	-- | Constrained types.
	--   Used in the solver only. When converting to core we add type class contexts
	--   as kinds using TForall, and more-than contraints as bounded quantification.
	| TConstrain	Type	Constraints
			
	-- | Used in the solver only.
	--   Represents an error in the type.
	| TError	Kind	TypeError
	deriving (Show, Eq)


-- | A binding occurrence of a variable.
data Bind
	-- | No binding.
	= BNil

	-- | Unbounded quantification.
	| BVar	Var
	
	-- | Bounded quantification. Type of bound variable must be more-than the given constraint.
	| BMore	Var Type
	deriving (Show, Eq)


-- | A bound occurrence of a variable.
--	These can have several forms depending on where we're using them.
data Bound
	-- | A regular variable.
	= UVar		Var

	-- | Used in core types only. 
	--   A type variable with an embedded more-than constraint. 
	--   It will have been bound by a `BMore` in an enclosing scope.
	| UMore		Var Type

	-- | Used in the kinds of witness constructors only.
	--   A de Bruijn index.
	| UIndex 	Int

	-- | Used in the solver only.
	--   A reference to some equivalence class in the type graph.
	--   Also known as a meta type variable.
	| UClass 	ClassId
	deriving (Show, Eq)


-- | Constraints used in the `TConstrain` constructor of `Type`.
--   The crsOther list should only contain FConstraint and FProj fetters.
data Constraints
	= Constraints 
	{ crsEq		:: Map Type Type
	, crsMore	:: Map Type Type
	, crsOther	:: [Fetter] }
	deriving (Show, Eq)


-- | Stores information about a type error directy in a type.
--	Used in the `TError` constructor of `Type`.
--	Used during type inference only.
data TypeError

	-- | General type error. Probably something couldn't be unified.
	= TypeError

	-- | A recursive type. 
	--   Equivalent to @(MU a. t2)@, where a can appear in t2.
	--   Recursive value types are treated as errors, as we can't represent
	--   them in the core language.
	| TypeErrorLoop	 Type Type	
	deriving (Show, Eq)


-- | A Fetter is a piece of type information which isn't part of the type's shape.
--   TODO: Refactor to only contain FConstraint 
data Fetter

	-- | A type class constraint.
	= FConstraint	Var	[Type]

	-- | t1 is equal to t2, and must be represented as a TVar or TClass.
	| FWhere	Type	Type

	-- | t1 is more than t2, and must be represented as a TVar or TClass.
	| FMore		Type	Type

	-- | A projection constraint.
	--   TODO: refactor this into a special constructor, and make FConstraint
	--	   above take that constructor instead of a plain var.
	| FProj		TProj	
			Var 	-- var to tie the instantiated projection function to.
			Type 	-- type of the dictionary to choose the projection from.
			Type 	-- type to unify the projection function with, once it's resolved.
				
	deriving (Show, Eq, Ord)


-- | Represents field and field reference projections.
--	TODO: merge this into the new FProj constructor.
data TProj
	-- | A field projection. @foo.fieldLabel@
	= TJField  !Var

	-- | A field reference projection. @foo.#fieldLabel@
	| TJFieldR !Var

	-- | Indexed field projection. @foo.INT@
	| TJIndex  !Var

	-- | Indexed field reference projection	@foo.#INT@
	| TJIndexR !Var
	deriving (Show, Eq, Ord)
	
	
-- Instances --------------------------------------------------------------------------------------
instance Ord Type where
 compare t1 t2
	| TVar _ b1	<- t1
	, TVar _ b2	<- t2
	= compare b1 b2
	
	| otherwise
 	= panic "Type.Exp" 
	$ "compare: can't compare type for ordering\n"
	% "    t1 = " % show t1	% "\n"
	% "    t2 = " % show t2 % "\n"


instance Ord Bound where
 compare b1 b2
	| UClass cid1	<- b1
	, UClass cid2	<- b2
	= compare cid1 cid2
	
	| Just v1	<- takeVarOfBound b1
	, Just v2	<- takeVarOfBound b2
	= compare v1 v2

	| Just o1	<- takeOrdOfBound b1
	, Just o2	<- takeOrdOfBound b2
	= compare o1 o2

	| otherwise
 	= panic "Type.Exp" 
	$ "compare: can't bound type for ordering\n"
	% "    t1 = " % show b1	% "\n"
	% "    t2 = " % show b2 % "\n"
 

	-- Define these locally to break import loops.
	where
	 takeOrdOfBound :: Bound -> Maybe Int
	 takeOrdOfBound bb
 	  = case bb of
		UClass{}	-> Just 0
		UVar{}		-> Just 1
		UMore{}		-> Just 2
		_		-> Nothing

	 takeVarOfBound :: Bound -> Maybe Var
	 takeVarOfBound bb
 	  = case bb of
		UVar v		-> Just v
		UMore v _	-> Just v
		_		-> Nothing


