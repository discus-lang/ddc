
-- | Core Language IR.
module Core.Exp
	( Tree
	, Top 		(..)	-- top level things
	, DataField 	(..)	-- data fields
	, CtorDef	(..)	-- constructor definitions
	, ClassContext	(..)	-- type class context
	, Exp 		(..)	-- expressions
	, Proj		(..)	-- projections
	, Prim		(..)	-- primitive functions
	, Op		(..)	-- primitive operators
	, Stmt	 	(..)	-- statements
	, Alt 		(..)	-- case/match alternatives
	, Guard		(..)	-- alternative guards
	, Pat		(..)	-- guard patterns
	, Label		(..))	-- labels in guards
where
import Util
import Shared.Exp
import Type.Exp
import DDC.Base.SourcePos
import DDC.Base.Literal
import DDC.Var


-- Trees and Globs -----------------------------------------------------------------------------------
-- | A flat list of Tops is the lowest common denominator for program representation.
type Tree	= [Top]


-- Top ---------------------------------------------------------------------------------------------
-- | Top level declarations.
--	The order of the constructors in this data type is also the standard order
--	that should appear in dumps and interface files.
--
data Top
	= -- | An abstract type class
	  PClass
		{ topClassName 		:: Var
		, topClassSuper 	:: Super }

	-- | A custom global effect constructor
	| PEffect
		{ topEffectName 	:: Var
		, topEffectKind 	:: Kind }

	-- | A top level\/global region.
	| PRegion
		{ topRegionName 	:: Var 
		, topRegionWitnesses 	:: [(Var, Type)] }

	-- | An unboxed data type imported from somewhere else.
	| PExternData
		{ topExternDataName	:: Var
		, topExternDataKind	:: Kind }

	-- | A (value) binding imported from somewhere else.
	| PExtern
		{ topExternName		:: Var
		, topExternType 	:: Type
		, topExternOpType	:: Type	}
	
	-- | A data type and its constructors.
	| PData
		{ topDataName		:: Var
		, topDataCtors		:: Map Var CtorDef }

	-- | A (value) type class dictionary.
	| PClassDict
		{ topClassDictName	:: Var
		, topClassDictParams	:: [(Var, Kind)]
		, topClassDictTypes	:: [(Var, Type)] }

	-- | A (value) type class instance.
	| PClassInst
		{ topClassInstName	:: Var
		, topClassInstArgs	:: [Type]
		, topClassInstMembers	:: [(Var, Exp)] }

	-- | A top-level binding.
	| PBind
		{ topBindName		:: Var
		, topBindExp		:: Exp }	
	deriving (Show, Eq)


-- | Meta-data about a constructor.
--	Note that we need to remember the indicies of each field so we can convert
--	pattern matches using labels to Sea form. 
--
data CtorDef
	= CtorDef 
	{ ctorDefName	:: Var 		-- ^ name of constructor
	, ctorDefType	:: Type		-- ^ type of constructor
	, ctorDefArity	:: Int		-- ^ arity of constructor
	, ctorDefTag	:: Int		-- ^ tag of constructor
	, ctorDefFields	:: Map Var Int  -- ^ map of field names to indexes in the constructor.
	}
	deriving (Show, Eq)


-- | A type class context.
data ClassContext
	= ClassContext Var [Var]
	deriving (Show, Eq)


-- Exp ---------------------------------------------------------------------------------------------
-- Core Expressions
data Exp
	-- Common Fragment ----------------------------------------------------
	-- These constructors are used in all stages.

	-- | A variable with its type
	= XVar		Var	Type

	-- | A literal value
	| XLit		LiteralFmt

	-- | Type abstraction.
	| XLAM		Bind 	Kind	Exp

	-- | Type application.
	| XAPP		Exp	Type

	-- | Value abstraction
	| XLam		Var	Type	Exp  Effect Closure

	-- | Value application
	| XApp		Exp	Exp	Effect

	-- | Do expression, contains stmts to execute.
	| XDo		[Stmt]

	-- | Matching and branching.
	| XMatch	[Alt]

	-- | Introduce a local region (letregion)
	| XLocal	Var	[(Var, Type)] Exp

	-- | Some primitive function
	| XPrim		Prim 	[Exp]

	-- | A type annotation
	| XTau		Type	Exp


	-- Special Purpose Constructors ---------------------------------------
	-- These are only used in specific stages.
	-- It would be better to refactor these into a common (XAnnot a) constructor
	-- 	and abstract over the annotation type.

	-- | Nil expression.
	--	XNil is used internally by compiler stages as a place holder for
	--	information that isn't present at the moment. If Core.Lint finds
	--	any XNil's _after_ a stage has completed then it will complain.
	| XNil

	-- An unresolved projection. 
	--	These come from the Desugared IR and are rewritten to 
	--	real function calls by Core.Dictionary.
	| XProject Exp Proj		
	
	-- Used to represent flattened value or type applications
	-- Used in Core.Dictionary (and others?)
	| XAppF   [Exp]
	| XAppFP  Exp  (Maybe Effect)
	| XType	  Type

	-- Used by Desugar.ToCore
	| XAt	 Var   Exp
				
	-- Used by Core.Lift
	--  	Place holder for a lambda abstraction that was lifted out
	-- 	name of lifted function. 
	--	Name of supercombinator, vars which were free in lifted expression.
	| XLifted Var [Var]			

	deriving (Show, Eq)


-- Proj --------------------------------------------------------------------------------------------
-- Field projections
data Proj
	= JField  Var			-- ^ A field projection.   		(.fieldLabel)
	| JFieldR Var			-- ^ A field reference projection.	(#fieldLabel)
	deriving (Show, Eq)


-- Prim --------------------------------------------------------------------------------------------
-- | Primitive Functions
data Prim
	-- laziness
	= MSuspend	Var		-- ^ Suspend some function	function name
	| MForce			-- ^ Force an expression	(expr list should have a single elem)

	-- a primitive operator
	| MOp		Op

	-- boxing and unboxing
	| MBox
	| MUnbox
	
	-- function calls
	-- 	move this to MCall
	| MTailCall	 		-- ^ Tailcall a super
	| MCall				-- ^ Call a super
	| MCallApp	Int		-- ^ Call then apply super with this airity.
	| MApply			-- ^ Apply a thunk.
	| MCurry	Int		-- ^ Build a thunk with this airity.
	deriving (Show, Eq)


-- Op ----------------------------------------------------------------------------------------------
-- | Primitive operators
--	We might do without this if we had a general expression rewrite system in place.
--	Then again, the fact that these operators are polymorphic makes it easy to write
--	compiler code that works with every primitive type..

data Op
	-- arithmetic
	= OpNeg				-- negation
	| OpAdd				-- addition
	| OpSub				-- subtraction
	| OpMul				-- multiplication
	| OpDiv				-- division
	| OpMod				-- modulus

	-- comparison
	| OpEq				-- equality
	| OpNeq				-- not equality
	| OpGt				-- greater than
	| OpGe				-- greater than or equal
	| OpLt				-- less than
	| OpLe				-- less than or equal
	
	-- boolean
	| OpAnd				-- and
	| OpOr				-- or
	deriving (Show, Eq)


-- Stmt --------------------------------------------------------------------------------------------
-- Statements
data Stmt
	= SBind  	(Maybe Var) Exp	-- ^ Let binding.
	deriving (Show, Eq)


-- Alt ---------------------------------------------------------------------------------------------
-- Match alternatives
data Alt
	= AAlt		[Guard] Exp
	deriving (Show, Eq)

data Guard
	= GExp		Pat	Exp	-- ^ Match against an auxilliary value.
	deriving (Show, Eq)

data Pat
	= WVar	Var			-- ^ Bind a variable
	| WLit	SourcePos LiteralFmt	-- ^ Match against a literal value
	| WCon				--   Match against a constructor and bind arguments.
		SourcePos 
		Var 
		[(Label, Var, Type)]	
	deriving (Show, Eq)
	
data Label
	= LIndex	Int		-- ^ i'th field of constructor.
	| LVar		Var		-- ^ a field name.
	deriving (Show, Eq)

