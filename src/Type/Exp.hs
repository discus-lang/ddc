
-- | Type Expressions
module Type.Exp
	( Var
	, Bind		(..)
	, Constraints	(..)

	-- super kinds
	, Super		(..)

	-- kinds
	, Kind		(..)
	, KiCon		(..)
	, kValue, kRegion, kEffect, kClosure
	, kMutable, kMutableT
	, kConst,   kConstT
	, kLazy,    kLazyH
	, kDirect
	, kPure
	, kEmpty

	-- types
	, Type		(..)
	, TyCon		(..)
	, TyClass	(..)
	, tPure, tEmpty

	, Index

	, ClassId	(..)
	, TProj		(..)
	, Fetter  	(..)
	, Elaboration	(..)

	, Data
	, Region
	, Effect
	, Closure
	, Witness

	, InstanceInfo (..))

where

import Util
import Shared.Var		(Var)
import Shared.VarPrim
import Shared.Error
import Data.Ix
import qualified Shared.Var	as Var

import Data.Map			(Map)
import Data.Set			(Set)

-- ClassId -----------------------------------------------------------------------------------------
--	A unique name for a particular type/region/effect equivalence class.
--
newtype ClassId	
	= ClassId Int
	deriving (Show, Eq, Ord)

instance Ix ClassId where
 range	   (ClassId a, ClassId b) 		= map ClassId [a..b]
 index	   (ClassId a, ClassId b) (ClassId x)	= x
 inRange   (ClassId a, ClassId b) (ClassId x)	= inRange (a, b) x
 rangeSize (ClassId a, ClassId b)		= rangeSize (a, b)
 

-- ordering
--	so we can use types containing class / var as keys in maps
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

	-- | ([]) The superkind of some other non-property encoding kind.
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
	--	    were '%' is a kind, but '+' and '% -> +' are superkinds.
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
	= KNil				-- ^ An missing / unknown kind.

	-- | Kind constructor
	| KCon	   KiCon Super		-- ^ Kind constructors

	--  | Dependent kind abstraction.
	--	Uses de Bruijn indicies in the body.
	| KPi      Kind  Kind		-- ^ Dependend kind abstraction.
				 	--	Uses de Bruijn indicies in the body

	--  | Dependent kind application.
	| KApp	   Kind	 Type

	-- Old stuff that needs refactoring -----------------------------------
	| KForall  Kind Kind		-- ^ Dependent kinds.
	| KFun     Kind Kind		-- ^ Function kinds. Equivalent to (forall (_ :: k). k)

	-- TODO: change TClass to be a general kind constructor, and TWitJoin to be a kind sum.
	| KClass	TyClass [Type]	-- ^ the kind of witnesses
	| KWitJoin	[Kind]		-- ^ joining of witnesses

	deriving (Show, Eq)	


-- | Kind constructors.
--	TODO: 	These aren't used yet
--		We need to replace the ctors in the Kind type with references to these ones.
data KiCon
	-- ^ A Witness Kind Constructor / Type Class Constructor defined in the source.
	--	These aren't interpreted in any special way by the compiler.
	= KiCon Var

	-- Built-in witness kind constructors ---------------------------------
	--	These have special meaning to the compiler.	

	-- Each of these base kinds are also their own superkind.
	-- For example: * :: *  and % :: %
	| KiConValue			-- the kind of value types
	| KiConRegion			-- the kind of region types
	| KiConEffect			-- the kind of effect types
	| KiConClosure			-- the kind of closure types
	
	-- | Mutability of regions.
	| KiConMutable			-- for a single region.
	| KiConMutableT			-- for all the regions in a type.

	-- | Constancy of regions.
	| KiConConst			-- for a single region
	| KiConConstT			-- for all the regions in a type.

	-- | Region might contain thunks.
	| KiConLazy			-- for a single region
	| KiConLazyH			-- for the primary region of a type.

	-- | Region does not contain thunks.
	| KiConDirect			-- for a single region.
		
	-- | Given effect is pure.
	| KiConPure
	
	-- | Given closure is empty.
	| KiConEmpty
	deriving (Show, Eq)


-- Short names for commonly used kinds
kValue		= KCon KiConValue	SBox
kRegion		= KCon KiConRegion	SBox
kEffect		= KCon KiConEffect	SBox
kClosure	= KCon KiConClosure	SBox

kMutable	= KCon KiConMutable	(SFun kRegion  SProp)
kMutableT	= KCon KiConMutableT	(SFun kValue   SProp)

kConst		= KCon KiConConst	(SFun kRegion  SProp)
kConstT		= KCon KiConConstT	(SFun kValue   SProp)

kLazy		= KCon KiConLazy	(SFun kRegion  SProp)
kLazyH		= KCon KiConLazyH	(SFun kValue   SProp)

kDirect		= KCon KiConDirect	(SFun kRegion  SProp)

kPure		= KCon KiConPure	(SFun kEffect  SProp)
kEmpty		= KCon KiConEmpty	(SFun kClosure SProp)


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
	--	We're currently moving the representation from TFetters to TConstrain.
	--	TConstrain uses finite maps, not unsorted lists, so is much more efficient.
	--
	| TFetters	Type	[Fetter]	-- ^ Holds extra constraint information.
	| TConstrain	Type	Constraints	-- ^ Holds extra constraint information.
			
	| TApp		Type	Type		-- ^ Type application.

	| TSum		Kind 	[Type]		-- ^ A summation, least upper bound.

	| TCon		TyCon			-- ^ A type constructor.
	| TVar     	Kind 	Var		-- ^ A type variable.

	| TTop		Kind			-- ^ Valid for Effects (!SYNC) and Closures ($OPEN) only.
	| TBot		Kind			-- ^ Valid for Effects (!PURE) and Closures ($EMPTY)
						--	also used in the inferencer to represent the type of an equivalence
						--	class that has not been constrained by a constructor.
	
	-- Effect and closure constructors are always fully applied..
	| TEffect	Var [Type]		-- ^ An effect constructor
	| TFree		Var Type		-- ^ An tagged object which is free in the closure.
						--	The tag should be a Value var.
						--	The type parameter should be a value type, region or closure.

	| TDanger	Type Type		-- ^ If a region is mutable then free type variables in the 
						--	associated type must be held monomorphic.
	
	-- Type sugar.
	--	Used in source and desugar stages only.
	| TElaborate	Elaboration Type

	-- Helpers for type inference.
	---	Used in type inference stages only.
	| TClass   Kind ClassId			-- ^ A reference to some equivalence class.
	| TError   Kind [Type]			-- ^ Classes with unification errors get their queues set to [TError].
	| TFetter  Fetter			-- ^ Holds a fetter, so we can put it in an equivalence class.

	-- A type variable with an embedded :> constraint.
	--	Used in core only, so we can reconstruct the type of an expression
	--	without having to see the bounds on enclosing foralls.
	| TVarMore	Kind	Var	Type

	-- A debruijn index
	--	Used in core only, in the kinds for witness constructors.
	| TIndex	Int

	-- Witness Joining
	--	Used in core only.
	--	We could perhaps create a family of specific joining functions
	--	instead but dealing with all the different combinations of argument
	--	types would be too much pain..
	| TWitJoin	[Witness]

	deriving (Show, Eq)

tPure	= TBot kEffect
tEmpty	= TBot kClosure

type Data	= Type
type Region	= Type
type Effect	= Type
type Closure	= Type
type Witness	= Type


-- Helps with defining foreign function interfaces.
data Elaboration
	= ElabRead					-- ^ read from some object
	| ElabWrite					-- ^ write to some object
	| ElabModify					-- ^ read and write some object
	deriving (Show, Eq)


-- TyCon -------------------------------------------------------------------------------------------
-- | Type constructors
data TyCon
	-- Function type constructor.
	= TyConFun

	-- A data type constructor.
	| TyConData
		{ tyConName	:: !Var
		, tyConDataKind	:: !Kind }

	-- Constructs a witness to some type/region/effect/closure class.
	| TyConClass
		{ tyConClass	 :: !TyClass
		, tyConClassKind :: !Kind }

	deriving (Show, Eq)


-- TyClass -----------------------------------------------------------------------------------------
-- | Type / Region / Effect classes.
--	As most of the type-level witnesses have the same names as
--	their kind-level classes, we'll overload TyClass for both.
--
data TyClass
	-- Various built-in classes.
	--	These have special meaning to the compiler.

	-- region classes	-- type classes
	= TyClassConst		| TyClassConstT
	| TyClassMutable	| TyClassMutableT
	| TyClassLazy		| TyClassLazyH
	| TyClassDirect

	-- purification
	| TyClassPurify		-- witness
	| TyClassPure		-- class

	-- empty closures
	| TyClassEmpty

	-- Some user defined class
	| TyClass Var
	deriving (Show, Eq)


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


-- InstanceInfo ------------------------------------------------------------------------------------
-- | Records information about how the type of a bound var was determined.
--	These carry information about instances of forall bound vars between the 
--	solver and Desugar.toCore so it can fill in type parameters.
--
--	InstanceInfo's are constructed by the constraint solver when that particular
--	var is encountered, so depending on how it was bound, the type of the var
--	may or may not be known.
--
--	Once solving is finished, the exporter can fill in the any missing types.
--	
--	InstanceInfo is highly polymorphic so that both Core and Type lands can fill
--	it with their own particular representations.
--
data InstanceInfo param t

	-- | An instance of a lambda bound variable
	= InstanceLambda
		Var 		-- the var of the use
		Var		-- binding var
		(Maybe t)	-- type of the bound var
	
	-- | A non-recursive instance of a let binding
	| InstanceLet
		Var		-- the var of the use
		Var		-- the binding var
		[param]		-- types corresponding to instantiated type vars
		t		-- the type of the scheme that was instantiated

	-- | A recursive instance of a let binding
	| InstanceLetRec
		Var		-- the var of the use
		Var		-- the binding var
		(Maybe t)	-- the type of the bound var

	deriving Show


