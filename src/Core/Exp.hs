
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
	, Prim		(..)	-- primitive operators
	, Stmt	 	(..)	-- statements
	, Annot 	(..)	-- expression annotations
	, Alt 		(..)	-- case/match alternatives
	, Guard		(..)	-- alternative guards
	, Pat		(..)	-- guard patterns
	, Label		(..)	-- labels in guards

	, Type 		(..)	-- core types

	, Data			-- alias of Type
	, Region
	, Effect
	, Closure
	, Class	

	, Kind		(..))	-- kind expressions

where

-----
import Util

-----
import Shared.Var (Var)
import Shared.Literal (Const)
import Shared.Exp

import Data.Set		(Set)

-----
type Tree	= [Top]


-----------------------
-- Top
-- | 	Top level expression.
--
data Top
	= PNil
	| PBind		Var Exp				-- ^ A Top level binding.
	| PExtern	Var Type Type			-- ^ A function or effect imported from somewhere else.
							-- 	name, value type, operational type
	| PData		Var 				--  Type name.
			[Var] 				--  Type vars.
			[CtorDef]			--  Constructor defs

	| PCtor		Var Type Type			-- ^ Constructor: ctor name, value type, operational type

	| PRegion	Var
	| PEffect	Var Kind			-- ^ A global effect.
	| PClass	Var Kind

	| PClassDict	Var [Type] [ClassContext] [(Var, Type)]
	| PClassInst	Var [Type] [ClassContext] [(Var, Exp)]

	deriving (Show, Eq)

data CtorDef 
	= CtorDef Var [DataField Exp Type]
	deriving (Show, Eq)

data ClassContext
	= ClassContext Var [Var]
	deriving (Show, Eq)

-----------------------
-- Exp
--
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
	| XMatch	[Alt]	Effect			-- ^ Matching of constructors and constants with effects.
	| XConst	Const	Type			-- ^ Literals.
	| XVar		Var	  			-- ^ A variable.
	| XLocal	Var	[(Var, Type)] Exp	-- ^ Introduce a local region.
	| XPrim		Prim 	[Exp]	Effect


	-- ditch these
	| XAtom	    Var	      [Exp]			-- ^ Reference an Atom. 	name, type args.
							--	Atoms are constructors of zero arity, eg Nil, Nothing.

	------
	-- Intermediate construcors
	--	These should not escape out of the modules that make use of them.
	--
	
	-- Used in Core.CrushApps
	| XType	  Type
	| XAppF   [Exp]
	| XAppFP  Exp  (Maybe Effect)

	-- Used by Desugar.ToCore
	| XAt	 Var   Exp
				
	-- Used by Core.Lift
	| XLifted Var [Var]				-- ^ Place holder for a lambda abstraction that was lifted out
							-- 	name of lifted function. 
							--	Name of supercombinator, vars which were free in lifted expression.

	deriving (Show, Eq)


data Prim
	-- laziness
	= MSuspend	Var				-- ^ Suspend some function	function name
	| MForce					-- ^ Force an expression	(expr list should have a single elem)

	-- boxing
	| MBox		Type Type			-- ^ Boxing.	boxed type, 	unboxed type
	| MUnbox	Type Type			-- ^ Unboxing.	unboxed type, 	boxed type
	
	-- function calls
	| MTailCall	Var 				-- ^ Tailcall a super
	| MCall		Var				-- ^ Call a super
	| MCallApp	Var Int				-- ^ Call then apply super. 	name, airity
	| MApply	Var				-- ^ Apply a thunk. 		thunk name, args
	| MCurry	Var Int				-- ^ Build a thunk. 		name, airity

	-- some other named primitive function.
	| MFun		Var  Type 			-- ^ Primitive operation.	opName, resultType
	deriving (Show, Eq)


-----------------------
-- Stmt
-- |	Statments,  sequencing.
--
data Stmt
--	= SComment 	String
	= SBind  	(Maybe Var) Exp			-- ^ Let binding.
	deriving (Show, Eq)


-----------------------
-- Alt
-- |	A match alternative.
--		A default alternative has no guards.
data Alt
	= AAlt		[Guard] Exp
	deriving (Show, Eq)

data Guard
	= GExp		Pat	Exp			-- ^ Match against an auxilliary value.
	deriving (Show, Eq)

data Pat
	= WConst	Const				-- ^ Match against a constant.
	| WCon		Var	[(Label, Var, Type)]	-- ^ Match against a constructor and bind arguments.
	deriving (Show, Eq)
	
data Label
	= LIndex	Int				-- ^ i'th field of constructor.
	| LVar		Var				-- ^ a field name.
	deriving (Show, Eq)



-----------------------
-- Type
--
data Type
	-------
	-- Type/kind/effect constructs.
	--
	= TNil

	| TForall	Bind	Kind	Type		-- ^ Type abstraction.
	| TContext		Kind	Type		-- ^ Class context.
	| TWhere	Type	[(Var, Type)]		-- ^ Type level where expression.
	| TApp		Type 	Type			-- ^ Type application.

	| TSum		Kind 	[Type]			-- ^ A summation \/ lub.
	| TMask		Kind	Type Type
	| TVar		Kind 	Var

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
	
	-- class witness
	| TClass	Var [Type]
	
	-- wildcards
	| TWild		Kind				-- ^ Type wildcard. 
							--	Will unify with anything of the given kind.
	deriving (Show, Eq)


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

------------------------
type Data	= Type
type Effect	= Type
type Closure	= Type
type Region	= Type
type Class	= Type

-----------------------
-- Kind
--
data Kind	
	= KNil

	| KData
	| KRegion
	| KEffect
	| KClosure
	| KFun 		Kind	Kind
	| KClass	Var	[Type]


	-- this is really a superkind, but we'll keep it here
	--	to discriminate witness variables from DREC vars
	| KWitness

	deriving (Show, Eq)


-----------------------
-- Annot
-- |	Expression annotations.
--	Used internally, and for debugging.
--
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
	
	


	
