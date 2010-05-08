
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
	, Elaboration	(..)
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


-- Super-Kinds ----------------------------------------------------------------------------------
data Super
	-- | (+) "prop" Properties. 
	--   The occurrence of a type level witness who's superkind has this as the result,
	--   guarantees some property of a program. 
	--   eg: MkConst %r1 :: Const %r1 :: +
	= SProp			

	-- | ([]) "box" The super-kind of some non-property encoding kind. 
	--   eg: 5 :: Int :: * :: []
	| SBox			
	
	-- | Super-Kind functions.
	| SFun Kind Super	
	deriving (Show, Eq)


-- Kinds ------------------------------------------------------------------------------------------
data Kind
	-- | Missing or as-yet-unknown kind information.
	= KNil

	-- | Kind constructor.
	| KCon	   KiCon Super

	--  | Dependent kind abstraction. Equivalent to (PI k1. k2)
	--    The body can contain de Bruijn indices (TIndex) that refer to the parameter type.
	| KFun     Kind  Kind

	--  | Dependent kind application.
	| KApp	   Kind	 Type

	-- Old stuff that needs refactoring -----------------------------------
	-- Factor this out. If we move to represent TWitJoin as TSum then that's 
	-- already got the kind encoded in it and we won't need this constructor.
	| KWitJoin	[Kind]		-- ^ Joining of witnesses
	deriving (Show, Eq)	


-- Type --------------------------------------------------------------------------------------------
data Type	
	-- | Missing or as-yet-unknown type information. 
	= TNil

	-- | Quantification
	--   TODO: Refactor TContext into TForall, using a new `Bind` constructor BNil.
	--         TContext is equivalent to (forall (_ :: k). t)
	| TForall	Bind 	Kind	Type
	| TContext		Kind	Type

	-- Constrained types.
	-- TODO: We are currently moving the representation from TFetters to TConstrain.
	-- TConstrain uses finite maps, not unsorted lists, so is much more efficient.
	| TFetters	Type	[Fetter]	-- ^ Holds extra constraint information.
	| TConstrain	Type	Constraints	-- ^ Holds extra constraint information.
			
	-- | Type application.
	| TApp		Type	Type		-- ^ Type application.

	-- | A type summation \/ least upper bound.
	| TSum		Kind 	[Type]

	-- | A type constructor.
	| TCon		TyCon

	-- | A totally constrained type.
	--   Only used as an effect, where it represents a computation that must
	--   be synchronised with all others.
	| TTop		Kind
	
	-- | A completely unconstrained type.
	--   This is equivalent to TSum k {}, but we keep both forms because its convenient.
	| TBot		Kind
	
	-- | A type variable.
	| TVar     	Kind Var

	-- A type variable with an embedded :> constraint. Used in core types only.
	| TVarMore	Kind Var Type
	
	-- | A de Bruijn index. Used in the the kinds of witness constructors only.
	| TIndex	Kind Int
	
	
	-- Helpers used in the solver only --------------------------
	-- | A meta type variable. 
	--   A reference to some equivalence class in the type graph.
	| TClass   	Kind ClassId

	-- | Represents an error in the type.
	--   The TypeError says what went wrong.
	| TError	Kind TypeError

	-- Helpers used in source and desugarer only ----------------
	-- Refactor this to be application of a special constructor.
	| TElaborate	Elaboration Type


	-- Old stuff that needs factoring out.
	| TEffect	Var [Type]		-- ^ An effect constructor
	| TFree		Var Type		-- ^ A tagged object which is free in the closure.
						--	The tag should be a Value var.
						--	The type parameter should be a value type, region or closure.

	| TDanger	Type Type		-- ^ If a region is mutable then free type variables in the 
						--	associated type must be held monomorphic.

  	-- Witness Joining.
	-- Used in core language only.
	
	-- TODO: Refactor this to use TSum.
	--       ie, represent  MkPureJoin e1 e2 w1 w2
        --
	--      w1 :: Pure e1   w2 :: Pure e2
	--     ------------------------------
	--      (w1 \/ w2) :: Pure (e1 \/ e2)
	-- 
	| TWitJoin	[Witness]

	deriving (Show, Eq)


-- | This data type includes constructors for bona-fide type expressions, 
--	as well as various helper constructors used in parsing/printing and type inference.
data Bind
	= BVar	Var				-- ^ unbounded quantification.
	| BMore	Var Type			-- ^ bounded quantification. Type of v1 must be :> t2
	deriving (Show, Eq)


-- | Constraints that can act on a type.
--	Some functions that act on types, packType in particular, want to do lots 
--	of lookups of equality constraints. We don't want to keep them all in a big list..
data Constraints
	= Constraints 
	{ crsEq		:: Map Type Type
	, crsMore	:: Map Type Type
	, crsOther	:: [Fetter] }
	deriving (Show, Eq)


-- TODO: We don't use these consistently. Better to remove them.
type Data	= Type
type Region	= Type
type Effect	= Type
type Closure	= Type
type Witness	= Type


-- | Helps with defining foreign function interfaces.
--	Used in the TElaborate constructor of Type.
--	Used in source and desugared types only.
data Elaboration
	= ElabRead			-- ^ read from some object
	| ElabWrite			-- ^ write to some object
	| ElabModify			-- ^ read and write some object
	deriving (Show, Eq)


-- | Stores information about a type error directy in a type.
--	Used in the TError constructor of Type.
--	Used during type inference only.
data TypeError
	= TypeErrorUnify [Type]		-- ^ types that couldn't be unified
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


-- Fetter ------------------------------------------------------------------------------------------
-- | A Fetter is a piece of type information which isn't part of the type's shape.
--   This includes type classes, shape constraints, field constraints and effect constraints.
--
data Fetter
	= FConstraint	Var	[Type]		-- ^ Constraint between types.
	| FWhere	Type	Type		-- ^ Equality of types, t1 must be TVar or TClass
	| FMore		Type	Type		-- ^ t1 :> t2

	-- | projections
	| FProj		TProj	
			Var 	-- var to tie the instantiated projection function to.
			Type 	-- type of the dictionary to choose the projection from.
			Type 	-- type to unify the projection function with, once it's resolved.
				
	deriving (Show, Eq, Ord)


-- TProj -------------------------------------------------------------------------------------------
-- | Represents field and field reference projections.
--
data TProj
	= TJField  !Var				-- ^ A field projection.   		(.fieldLabel)
	| TJFieldR !Var				-- ^ A field reference projection.	(#fieldLabel)

	| TJIndex  !Var				-- ^ Indexed field projection		(.<int>)
	| TJIndexR !Var				-- ^ Indexed field reference projection	(#<int>)
	deriving (Show, Eq, Ord)


