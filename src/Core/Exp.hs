
-- | Type definitions for DDC core language.
module Core.Exp
	( Var
	, Bind		(..)	-- binders
	, varOfBind

	, Tree
	, Top 		(..)	-- top level things
	, DataField 	(..)	-- data fields
	, CtorDef	(..)	-- constructor definitions
	, Exp 		(..)	-- expressions
	, Lit		(..)	-- literal values
	, Proj		(..)	-- projections
	, Prim		(..)	-- primitive functions
	, Op		(..)	-- primitive operators
	, Stmt	 	(..)	-- statements
	, Annot 	(..)	-- expression annotations
	, Alt 		(..)	-- case/match alternatives
	, Guard		(..)	-- alternative guards
	, Pat		(..)	-- guard patterns
	, Label		(..)	-- labels in guards

	, Type 		(..)	-- core types
	, Fetter	(..)
	, varOfFetter

	, Data			-- alias of Type
	, Region
	, Effect
	, Closure
	, Class	
	, Witness

	, Kind		(..))	-- kind expressions

where

-----
import Util

-----
import Shared.Var (Var)
import Shared.Exp

import Data.Set		(Set)

-----
type Tree	= [Top]


-- Top ---------------------------------------------------------------------------------------------
-- Top level expressions

data Top
	= PNil
	| PBind		Var Exp				-- ^ A Top level binding.
	| PExtern	Var Type Type			-- ^ A function or effect imported from somewhere else.
							-- 	name, value type, operational type
	| PData		Var 				--  Type name.
			[Var] 				--  Type vars.
			[CtorDef]			--  Constructor defs

	| PCtor		Var Type Type			-- ^ Constructor: ctor name, value type, operational type

	| PRegion	Var [(Var, Type)]		-- ^ A global region.
	| PEffect	Var Kind			-- ^ A global effect.
	| PClass	Var Kind

	| PClassDict	Var [Type] [ClassContext] [(Var, Type)]
	| PClassInst	Var [Type] [ClassContext] [(Var, Exp)]

	deriving (Show, Eq)

data CtorDef 
	= CtorDef Var [DataField Var Type]
	deriving (Show, Eq)

data ClassContext
	= ClassContext Var [Var]
	deriving (Show, Eq)


-- Exp ---------------------------------------------------------------------------------------------
-- Expressions

data Exp
	= XNothing					-- ^ Empty expression
							-- 	In contrast to XNil, an XNothing represents some part of the
							--	tree with is _supposed_ to be empty between stages

	| XNil						-- ^ Nil expression.
							--	XNil is used internally by compiler stages as a place holder for
							--	information that isn't present at the moment. If Core.Lint finds
							--	any XNil's _after_ a stage has completed then it will complain.
	
	| XAnnot [Annot] Exp				-- ^ Annotation.


	------
	-- Core Constructs
	--
	| XLAM		Bind 	Kind	Exp		-- ^ Type\/region\/effect\/closure abstraction.
	| XAPP		Exp	Type			-- ^ Type\/region\/effect\/closure application.
	| XTet		[(Var, Type)]	Exp		-- ^ A type-level let binding.
	| XTau		Type	Exp			-- ^ A type annotation.

	| XLam		Var	Type	Exp  Effect Closure	-- ^ Value abstraction.	
	| XApp		Exp	Exp	Effect		-- ^ Value application.

	| XDo		[Stmt]				-- ^ Do expression.		TODO add Effect
	| XMatch	[Alt]				-- ^ Matching of constructors and constants with effects.
	| XLocal	Var	[(Var, Type)] Exp	-- ^ Introduce a local region.

	| XVar		Var	Type 			-- ^ A variable.
	| XLit		Lit				-- ^ A literal value

	| XPrim		Prim 	[Exp]


	| XType	  Type


	-- ditch these
	| XAtom	    	Var	[Exp]			-- ^ Reference an Atom. 	name, type args.
							--	Atoms are constructors of zero arity, eg Nil, Nothing.

	-- An unresolved projection. 
	--	These are written to real function calls by Core.Dictionary
	| XProject	Exp	Proj		

	------
	-- Intermediate construcors
	--	These should not escape out of the modules that make use of them.
	--
	
	-- Used in Core.CrushApps
	| XAppF   [Exp]
	| XAppFP  Exp  (Maybe Effect)

	-- Used by Desugar.ToCore
	| XAt	 Var   Exp
				
	-- Used by Core.Lift
	| XLifted Var [Var]				-- ^ Place holder for a lambda abstraction that was lifted out
							-- 	name of lifted function. 
							--	Name of supercombinator, vars which were free in lifted expression.

	deriving (Show, Eq)

-- Proj --------------------------------------------------------------------------------------------
-- Field projections

data Proj
	= JField  Var				-- ^ A field projection.   		(.fieldLabel)
	| JFieldR Var				-- ^ A field reference projection.	(#fieldLabel)
	deriving (Show, Eq)


-- Lit ---------------------------------------------------------------------------------------------
-- (unboxed) literal values

data Lit
	= LInt8		Integer
	| LInt16	Integer
	| LInt32	Integer
	| LInt64	Integer
	
	| LWord8	Integer
	| LWord16	Integer
	| LWord32	Integer
	| LWord64	Integer
	
	| LFloat32	Double
	| LFloat64	Double

	| LChar32	Char
	| LString	String
	deriving (Show, Eq)


-- Prim --------------------------------------------------------------------------------------------
-- | Primitive Functions

data Prim
	-- laziness
	= MSuspend	Var				-- ^ Suspend some function	function name
	| MForce					-- ^ Force an expression	(expr list should have a single elem)

	-- a primitive operator
	| MOp		Op

	-- boxing and unboxing
	| MBox
	| MUnbox
	
	-- function calls
	-- 	move this to MCall
	| MTailCall	 				-- ^ Tailcall a super
	| MCall						-- ^ Call a super
	| MCallApp	Int				-- ^ Call then apply super with this airity.
	| MApply					-- ^ Apply a thunk.
	| MCurry	Int				-- ^ Build a thunk with this airity.

	-- some other named primitive function.
	| MFun		-- Var  Type 			-- ^ Primitive operation.	opName, resultType
	deriving (Show, Eq)


-- Op ----------------------------------------------------------------------------------------------
-- | Primitive operators
--	We might do without this if we had a general expression rewrite system in place.
--	Then again, the fact that these operators are polymorphic makes it easy to write
--	compiler code that works with every primitive type..

data Op
	-- arithmetic
	= OpNeg		-- negation
	| OpAdd		-- addition
	| OpSub		-- subtraction
	| OpMul		-- multiplication
	| OpDiv		-- division
	| OpMod		-- modulus

	-- comparison
	| OpEq		-- equality
	| OpNeq		-- negative equality
	| OpGt		-- greater than
	| OpGe		-- greater than or equal
	| OpLt		-- less than
	| OpLe		-- less than or equal
	
	-- boolean
	| OpAnd		-- and
	| OpOr		-- or
	deriving (Show, Eq)


-- Stmt --------------------------------------------------------------------------------------------
-- Statements

data Stmt
	= SBind  	(Maybe Var) Exp			-- ^ Let binding.
	deriving (Show, Eq)


-- Alt ---------------------------------------------------------------------------------------------
-- Match alternatives

data Alt
	= AAlt		[Guard] Exp
	deriving (Show, Eq)

data Guard
	= GExp		Pat	Exp			-- ^ Match against an auxilliary value.
	deriving (Show, Eq)

data Pat
	= WLit		Lit				-- ^ Match against a literal value
	| WCon		Var	[(Label, Var, Type)]	-- ^ Match against a constructor and bind arguments.
	deriving (Show, Eq)
	
data Label
	= LIndex	Int				-- ^ i'th field of constructor.
	| LVar		Var				-- ^ a field name.
	deriving (Show, Eq)




-- Type --------------------------------------------------------------------------------------------
-- Type expressions

type Data	= Type
type Effect	= Type
type Closure	= Type
type Region	= Type
type Class	= Type
type Witness	= Type

data Type
	-------
	-- Type/kind/effect constructs.
	--
	= TNil

	| TForall	Bind	Kind	Type		-- ^ Type abstraction.
	| TContext		Kind	Type		-- ^ Class context.
	| TFetters	Type	[Fetter]		-- ^ Type level where expression.
	| TApp		Type 	Type			-- ^ Type application.

	| TSum		Kind 	[Type]			-- ^ A summation \/ lub.
	| TMask		Kind	Type 	Type

	| TVar		Kind 	Var			-- ^ a variable of the given kind
	| TVarMore	Kind 	Var	Type		-- ^ an effect/closure variable with a :> bound

	| TTop		Kind
	| TBot		Kind

	-- data
	| TData		Var [Type]			-- ^ A data constructor
	| TFunEC	Type Type Effect Closure	-- ^ A function with an effect and closure.
	| TFun		Type Type			-- ^ Functions without effect or closure information
							--	Used for operational types.
	-- effect
	| TEffect 	Var [Type]			-- ^ a manifest effect

	-- closure
	| TFree		Var Type			-- ^ some object free in the closure of a function.
	| TTag		Var
	
	-- witnesses
	| TClass	Var [Type]			-- ^ A witness to some class
	| TPurify	Effect Class			-- ^ A witness to purification of an effect
	| TPurifyJoin	[Class]				-- ^ Joining of purification witnesses

	| TWitJoin	[Class]				-- ^ Join all these witnesses
	
	-- wildcards
	| TWild		Kind				-- ^ Type wildcard. 
							--	Will unify with anything of the given kind.
	deriving (Show, Eq)

data Fetter
	= FWhere	Var Type
	| FMore		Var Type
	deriving (Show, Eq)

varOfFetter ff
 = case ff of
 	FWhere v t	-> v
	FMore  v t	-> v

-----------------------
-- TBind
--
data Bind
	= BVar	Var					-- ^ unbounded quantification.
	| BMore	Var Type				-- ^ bounded quantification. Type of v1 must be :> t2
	deriving (Show, Eq)

-- | slurp out the variable from a TBind
varOfBind :: Bind	-> Var
varOfBind (BVar v)	= v
varOfBind (BMore v t)	= v

-- | order TBinds by their var so we can use them as Set and Map keys.
instance Ord Bind where
 compare b1 b2		= compare (varOfBind b1) (varOfBind b2)


-- Kind --------------------------------------------------------------------------------------------
-- Kind expressions

data Kind	
	= KNil

	| KData						-- ^ the kind of value types  (change to KValue)
	| KRegion
	| KEffect
	| KClosure
	| KFun 		Kind	Kind
	| KClass	Var	[Type]

	| KWitJoin	[Kind]				-- ^ joining of witness kinds
	deriving (Show, Eq)


-- Annot -------------------------------------------------------------------------------------------
-- Expression annotations
--	TODO: A Lot of this is junk that isn't being used

data Annot
	= NString 	String				-- ^ Some string: for debugging.
	| NType   	Type 				-- ^ Gives the type for an expression.
	| NTypeOp	Type				-- ^ Gives the operational type for a supercombinator.
	| NUseCount	Int				-- ^ Count of how many times this exp\/binding is used in the code.
	| NPure						-- ^ Exp has no effects, and is pure.
	| NBindVar	Var				-- ^ Some var which is safe to use as a binding var for this exp.

	| NFreeLevel	[(Var, Int)]			-- ^ Some free vars with binding levels.
	| NVarSet	(Set Var)

	-- Used in Core.Optimise.FullLaziness
	| NLevel	Int				
	deriving (Show, Eq)
