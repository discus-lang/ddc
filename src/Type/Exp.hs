
-- | Type Expressions
module Type.Exp
	( module DDC.Type.ClassId
	, module DDC.Type.KiCon
	, module DDC.Type.TyCon
	
	, Bind		(..)
	, Constraints	(..)

	-- super kinds
	, Super		(..)

	-- kinds
	, Kind		(..)

	-- types
	, Type		(..)

	, Index

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


-- Super Kinds ----------------------------------------------------------------------------------
data Super
	-- | (+)  A kind-level object with this superkind describes some property of the program.
	---	  The occurance of some type-level object with this superkind /guarantees/ a 
	--	  property of a program
	--
	--	  Example:
	--	  'Mutable %r1' has superkind '+'
	--		It describes a property of the program, specifically, that objects
	--		in the region named '%r1' may be destructively updated.
	--
	--	  'MkConst %r1' has kind 'Const %r1' and superkind '+'
	--		If a 'MkConst %r1' occurs in the program then this guarantees that
	--		objects in region '%r1' will /not/ be destructively updated.
	--
	= SProp			

	-- | ([]) The superkind of some non-property encoding kind.
	--        Example:
	--	    '5'   has type      'Int'
	--	    'Int' has kind      '*'
	--	    '*'   has superkind '[]'
	--
	| SBox			
	
	-- | Properties relate types.
	--        Kind constructors take well-kinded types and produce properties.
 	-- 
	--        Example:	
	--        The kind-level constructor 'Mutable' has superkind % -> +,
	--	    where '%' is a kind, but '+' and '% -> +' are superkinds.
	--          This says that 'Mutable' encodes some property of a region.
	--
	| SFun	Kind Super	
	deriving (Show, Eq)


-- Kinds ------------------------------------------------------------------------------------------

-- We use de Bruijn indicies in the kinds of witness constructors.
type Index
	= Int

-- Kinds
data Kind
	=
	  -- | A missing kind annot.
	  --	TODO: Remove this if it's not being used.
	  KNil

	-- | Kind constructor
	| KCon	   KiCon Super

	--  | Dependent kind abstraction. 
	--	Types in the body can use de Bruijn indices (TIndex)
	--	to refer to the parameter.
	| KPi      Kind  Kind

	--  | Dependent kind application.
	| KApp	   Kind	 Type


	-- Old stuff that needs refactoring -----------------------------------
	| KForall  Kind Kind		-- ^ Dependent kinds. Same as KPi above. 
					--	Should rename all uses of KForall to KPi
	
	| KFun     Kind Kind		-- ^ Function kinds. Equivalent to (forall (_ :: k). k)
					--	Should rename all uses of KFun to KPi

	| KApps    Kind [Type]		-- ^ Flat dependent kind application.
					--	Should use KApp instead.
	
	| KWitJoin	[Kind]		-- ^ Joining of witnesses

	deriving (Show, Eq)	


-- Type --------------------------------------------------------------------------------------------
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

data Type	
	= TNil					-- ^ A hole. Something is missing.

	| TForall	Bind 	Kind	Type	-- ^ Type abstraction.
	| TContext		Kind	Type	-- ^ Class abstraction. Equivalent to (forall (_ :: k). t)

	-- constrained types.
	--	We are currently moving the representation from TFetters to TConstrain.
	--	TConstrain uses finite maps, not unsorted lists, so is much more efficient.
	--
	| TFetters	Type	[Fetter]	-- ^ Holds extra constraint information.
	| TConstrain	Type	Constraints	-- ^ Holds extra constraint information.
			
	| TApp		Type	Type		-- ^ Type application.

	| TSum		Kind 	[Type]		-- ^ A summation, least upper bound.

	| TCon		TyCon			-- ^ A type constructor.
	| TVar     	Kind 	Var		-- ^ A type variable.
	| TIndex	Int			-- ^ A debruijn index. Used in the kinds of witness constructors.

	| TTop		Kind			-- ^ Valid for Effects (!SYNC) and Closures ($OPEN) only.
	| TBot		Kind			-- ^ Valid for Effects (!PURE) and Closures ($EMPTY)
						--	also used in the inferencer to represent the type of an equivalence
						--	class that has not been constrained by a constructor.
	
	| TEffect	Var [Type]		-- ^ An effect constructor
	| TFree		Var Type		-- ^ A tagged object which is free in the closure.
						--	The tag should be a Value var.
						--	The type parameter should be a value type, region or closure.

	| TDanger	Type Type		-- ^ If a region is mutable then free type variables in the 
						--	associated type must be held monomorphic.
	
	-- Type sugar. 
	--	Used in source and desugar stages only.
	| TElaborate	Elaboration Type


	-- Helpers for type inference.
	--	Used in type inference stages only.
	| TClass   	Kind ClassId		-- ^ A reference to some equivalence class.
						--	Also known as a "meta" type variable.

	| TError	Kind TypeError		-- ^ Represents an error in the type. 
						--	The TypeError contains information about what went wrong.

	-- A type variable with an embedded :> constraint.
	--	Used in core types only.
	| TVarMore	Kind	Var	Type

	-- Witness Joining
	--	Used in core language only.
	--	We could perhaps create a family of specific joining functions
	--	instead but dealing with all the different combinations of argument
	--	types would be too much pain..
	| TWitJoin	[Witness]

	deriving (Show, Eq)

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
 compare   (TClass _ a)  (TClass _ b)		= compare a b
 compare   (TVar _ a)   (TVar k b)		= compare a b
 compare   (TClass{})   (TVar{})		= LT
 compare   (TVar{})     (TClass{})		= GT

 compare   t1           t2
 	= panic "Type.Exp" 
	$ "compare: can't compare type for ordering\n"
	% "    t1 = " % show t1	% "\n"
	% "    t2 = " % show t2 % "\n"


-- Fetter ------------------------------------------------------------------------------------------
-- | A Fetter is a piece of type information which isn't part of the type's shape.
--   This includes type classes, shape constraints, field constraints and effect constraints.
--
data Fetter
	= FConstraint	Var	[Type]			-- ^ Constraint between types.
	| FWhere	Type	Type			-- ^ Equality of types, t1 must be TVar or TClass
	| FMore		Type	Type			-- ^ t1 :> t2

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


