
module Type.Exp
	( Var
	, Type		(..)
	, ClassId	(..)
	, TProj		(..)
	, Fetter  	(..)
	, TypeSource 	(..)
	, Kind		(..)

	, Data
	, Region
	, Effect
	, Closure
	
	, InstanceInfo (..))

where

import Util
import Shared.Base		(SourcePos)
import Shared.Var		(Var)
import Shared.Literal
import Shared.Error
import Data.Ix
import qualified Shared.Var	as Var

-----------------------
-- ClassId
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
	$ "compare: can't compare types\n"
	% "    t1 = " % show t1	% "\n"
	% "    t2 = " % show t2 % "\n"




-----------------------
-- Kind
--
data Kind
	= KNil
	| KBox						
	| KFun	   Kind	Kind				-- ^ The kind of type constructors.	(->)
	| KData						-- ^ The kind of data.			(*)
	| KRegion					-- ^ The kind of regions.		(%)
	| KEffect					-- ^ The kind of effects.		(!)
	| KClosure					-- ^ The kind of closures.		($)
	| KFetter					-- ^ The kind of fetters.		(+)
	| KError					-- ^ An Error in the kind expression.
	deriving (Show, Eq)	

type Data	= Type
type Region	= Type
type Effect	= Type
type Closure	= Type

-----------------------
-- Type
--	| Type Expressions.
--	  This data type includes constructors for bona-fide type expressions, as well
--	  as various things used in parsing/printing and type inference.
--
data Type	
	= TNil						-- ^ A hole. Something is missing.

	| TForall	[(Var, Kind)] Type		-- ^ Universal quantifier.
	| TFetters	[Fetter]      Type		-- ^ Holds extra constraint information.

	| TSum		Kind 	[Type]
	| TUnify	Kind	[Type]
	| TMask		Kind	Type	Type		-- ^ Mask out some effects/vars from this effect/closure
	| TVar     	Kind 	Var			-- ^ A type variable.

	| TTop		Kind
	| TBot		Kind
	
	-- data
	| TData     	Var [Type]			-- ^ Type constructor. (ctor name, regions, args).
	| TFun          Type Type Effect Closure	-- ^ A function with an effect and environment.

	-- effect
	| TEffect	Var [Type]			-- ^ An effect constructor
	
	-- closure
	| TFree		Var Type			-- ^ An tagged object which is free in the closure. 
							--	The tag should be a Value var.
	
	| TTag		Var				-- ^ A tag for one of these objects, used in the RHS of TMask.

	-- wildcards	
	| TWild		Kind				-- ^ Type wildcard, can be unified with anything of the given kind.

	-- used in solver
	| TClass	Kind	ClassId			-- ^ A reference to some equivalence class.
	| TAccept	Type				-- ^ The eq class can only be this type.
	| TFetter	Fetter				-- ^ Holds a fetter, so we can put it in an equivalence class.
	| TError        Kind	Type			-- ^ Classes with unification errors get their queues set to [TError].

	| TNode		Type	Type			-- ^ A type along with its node name
							--	t1 should be TClass or TVar


	-- Type sugar, used during foreign import/export
	--	This is removed by the desugarer.
	--	Perhaps change this to (TElaborate Elaboration Type)
	--	and subsume TSig, TSigExact, TQuant
	| TElaborate	Type				-- ^ Elaborate a type by adding region/effect/closure information.
	| TMutable	Type				-- ^ This object is modified by the function.


	-- ???? Where is this stuff actually used? better to ditch it.
	--	Intermediate constructors.
	--	Different representations for things, used in lots of places.
	--
	| TFunF		[(Type, Effect, Closure)]	-- ^ Flat Function representation.
							-- 	The list of terms must have at least 2 elements.

	| TFunV	  	Type Type (Maybe Var)		-- ^ Labeled function representation
							--	Used in Type.Pretty

	deriving (Show, Eq)
	

-----------------------
-- Fetter
--	| A Fetter is a piece of type information which isn't part of the type's shape.
--	  This includes type classes, shape constraints, field constraints and effect constraints.
--
data Fetter
	= FConstraint	Var	[Type]			-- ^ Constraint between types.
	| FLet		Type	Type			-- ^ Equality of types, t1 must be TVar or TClass
	| FMore		Type	Type			-- ^ t1 :> t2

	-- | projection function is t1 = t2 -(eff clo)> t3
	| FProj		TProj	Var Type Type Effect Closure


	-- junk?
	-- Intermediate constructors.
	| FFunInfo	Var     Effect Closure		-- ^ Information about function labels.
							-- 	Used to help with pretty printing.

	deriving (Show, Eq)


-----------------------
-- TProj
--	| Represents field and field reference projections.
--
data TProj
	= TJField  Var				-- ^ A field projection.   		(.fieldLabel)
	| TJFieldR Var				-- ^ A field reference projection.	(#fieldLabel)

	| TJIndex  Var				-- ^ Indexed field projection		(.<int>)
	| TJIndexR Var				-- ^ Indexed field reference projection	(#<int>)
	deriving (Show, Eq)


-----------------------
-- TypeSource
--	| Records where constraints came from.
--	  These are useful for debugging and reporting the source of type error.
--
data TypeSource
	= TSNil


	| TSLiteral	SourcePos Const		-- ^ Literal value.

	| TSInst	Var Var 		-- ^ Constraint from an instantiated type scheme.
						--	(type var of definition, type var of instance)

	| TSProj	SourcePos TProj		-- ^ Field projection.

	| TSLambda	SourcePos		-- ^ Lambda abstraction.

	| TSApp		SourcePos 		-- ^ Function application.

	| TSMatchObj	SourcePos		-- ^ Match object.
	| TSMatchAlt	SourcePos		-- ^ Match alternatives.
	| TSMatch	SourcePos		-- ^ Match expression.

	| TSDo		SourcePos		-- ^ Constraint from do expression.

	| TSIfObj	SourcePos		-- ^ If object.
	| TSIfAlt	SourcePos		-- ^ If alternatives.
	| TSIf		SourcePos		-- ^ If expression.

	| TSStmt	SourcePos		-- ^ Statement.
	| TSGuard	SourcePos		-- ^ Chard.
					
	| TSSig		SourcePos Var 		-- ^ Type signature.

	| TSField	Var Var Var		-- ^ Field type signature within a data type declaration.
						--	(data type, constructor, field label)	

	| TSClassInst	SourcePos Var		-- ^ Class Instance
						--	(name of class)

	| TSData	SourcePos		-- ^ Data definition.
	| TSProjDict	SourcePos		-- ^ Projection dictionary.

	-----
	| TSSynth	Var			-- ^ Constraint synthesised by the solver in order to generate fresh names.
	| TSCrushed	Fetter			-- ^ Constraint from a crushed fetter
	| TSProjCrushed	ClassId ClassId TProj	-- ^ Constraint from a (crushed) field projection.
	| TSClassName				-- ^ Added by Class.makeClassName


	deriving (Show, Eq)


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
