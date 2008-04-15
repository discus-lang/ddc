
-- | Type Expressions
module Type.Exp
	( Var
	, Type		(..)
	, TyCon		(..)
	, ClassId	(..)
	, TProj		(..)
	, Fetter  	(..)
	, Kind		(..)
	, Elaboration	(..)

	, Data
	, Region
	, Effect
	, Closure

	, InstanceInfo (..))

where

import Util
import Shared.Var		(Var)
import Shared.Error
import Data.Ix
import qualified Shared.Var	as Var

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


-- Kind --------------------------------------------------------------------------------------------
data Kind
	= KNil						-- ^ An missing / unknown kind.
	| KFun	   Kind	Kind				-- ^ The kind of type constructors.	(->)
	| KValue					-- ^ The kind of value types.		(*)
	| KRegion					-- ^ The kind of regions.		(%)
	| KEffect					-- ^ The kind of effects.		(!)
	| KClosure					-- ^ The kind of closures.		($)
	| KFetter					-- ^ The kind of class constraints	(+)
	deriving (Show, Eq)	

type Data	= Type
type Region	= Type
type Effect	= Type
type Closure	= Type


-- Type --------------------------------------------------------------------------------------------
-- | This data type includes constructors for bona-fide type expressions, as well
--   as various things used in parsing/printing and type inference.
--
data Type	
	= TNil						-- ^ A hole. Something is missing.

	| TForall	[(Var, Kind)] Type		-- ^ Universal quantifier.
	| TFetters	[Fetter]      Type		-- ^ Holds extra constraint information.

	| TSum		Kind 	[Type]
	| TMask		Kind	Type	Type		-- ^ Mask out some effects/vars from this effect/closure
	| TVar     	Kind 	Var			-- ^ A type variable.

	| TApp		Type	Type			-- ^ General type application.
	| TCon		TyCon

	| TTop		Kind
	| TBot		Kind
	
	-- data
	| TData     	Kind Var [Type]			-- ^ A data type constructor.
	| TFun          Type Type Effect Closure	-- ^ A function with an effect and environment.

	-- effect
	| TEffect	Var [Type]			-- ^ An effect constructor
	
	-- closure
	| TFree		Var Type			-- ^ An tagged object which is free in the closure.
							--	The tag should be a Value var.
							--	The type parameter should be a value type, region or closure.

	| TDanger	Type Type			-- ^ If a region is mutable then free type variables in the 
							--	associated type must be held monomorphic.
	
	| TTag		Var				-- ^ A tag for one of these objects, used in the RHS of TMask.

	-- wildcards	
	| TWild		Kind				-- ^ Type wildcard, can be unified with anything of the given kind.

	-- used in solver
	| TClass	Kind	ClassId			-- ^ A reference to some equivalence class.
	| TFetter	Fetter				-- ^ Holds a fetter, so we can put it in an equivalence class.
	| TError        Kind	[Type]			-- ^ Classes with unification errors get their queues set to [TError].

	-- Type sugar, used during foreign import/export
	| TElaborate	Elaboration Type
	deriving (Show, Eq)


-- Helps with defining foreign function interfaces.
data Elaboration
	= ElabRead					-- ^ read from some object
	| ElabWrite					-- ^ write to some object
	| ElabModify					-- ^ read and write some object
	deriving (Show, Eq)


-- | Type constructors
data TyCon
	= TyConFun
		{ tyConKind	:: Kind }

	| TyConData
		{ tyConName	:: Var
		, tyConKind	:: Kind }

	deriving (Show, Eq)

-- Fetter ------------------------------------------------------------------------------------------
-- | A Fetter is a piece of type information which isn't part of the type's shape.
--   This includes type classes, shape constraints, field constraints and effect constraints.
--
data Fetter
	= FConstraint	Var	[Type]			-- ^ Constraint between types.
	| FLet		Type	Type			-- ^ Equality of types, t1 must be TVar or TClass
	| FMore		Type	Type			-- ^ t1 :> t2

	-- | projections
	| FProj		TProj	
			Var 	-- var to tie the instantiated projection function to.
			Type 	-- type of the dictionary to choose the projection from.
			Type 	-- type to unify the projection function with, once it's resolved.
				
	deriving (Show, Eq)


-- TProj -------------------------------------------------------------------------------------------
-- | Represents field and field reference projections.
--
data TProj
	= TJField  Var				-- ^ A field projection.   		(.fieldLabel)
	| TJFieldR Var				-- ^ A field reference projection.	(#fieldLabel)

	| TJIndex  Var				-- ^ Indexed field projection		(.<int>)
	| TJIndexR Var				-- ^ Indexed field reference projection	(#<int>)
	deriving (Show, Eq)


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


