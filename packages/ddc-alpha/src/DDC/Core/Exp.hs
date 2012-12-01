{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- | Core Language.
module DDC.Core.Exp
	( module DDC.Core.Exp.Prim
	, Tree
	, Top 		(..)	-- top level things
	, Exp 		(..)	-- expressions
	, Stmt	 	(..)	-- statements
	, Alt 		(..)	-- case/match alternatives
	, Guard		(..)	-- alternative guards
	, Pat		(..)	-- guard patterns
	, Label		(..))	-- labels in guards
where
import DDC.Core.Exp.Prim
import DDC.Base.SourcePos
import DDC.Base.Literal
import DDC.Type.Exp
import DDC.Type.Data.Base
import DDC.Var


-- Tree -------------------------------------------------------------------------------------------
-- | A flat list of top-level things is the lowest common denominator for program representation.
type Tree	= [Top]


-- Top ---------------------------------------------------------------------------------------------
-- | Top level declarations.
--	The order of the constructors in this data type is also the standard order
--	that should appear in dumps and interface files.
--	Note that the only declaration that contains Exps is PBind, which makes them
--	easy to find when doing transforms.
data Top
	= -- | An abstract type class.
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

	-- | A (value) binding imported from somewhere else.
	| PExtern
		{ topExternName		:: Var
		, topExternType 	:: Type
		, topExternOpType	:: Type	}

	-- | A data type and its constructors.
	| PData	{ topDataDef		:: DataDef }

	-- | A (value) type class dictionary.
	| PClassDict
		{ topClassDictName	:: Var
		, topClassDictParams	:: [(Var, Kind)]
		, topClassDictTypes	:: [(Var, Type)] }

	-- | A (value) type class instance.
	| PClassInst
		{ topClassInstName	:: Var
		, topClassInstArgs	:: [Type]
		, topClassInstMembers	:: [(Var, Var)] }

	-- | A top-level binding.
	| PBind
		{ topBindName		:: Var
		, topBindExp		:: Exp }
	deriving (Show, Eq)



-- Exp ---------------------------------------------------------------------------------------------
-- Core Expressions
data Exp
	-- | A place holder for a missing expresion, used during debugging.
	= XNil

	-- | A literal value.
	| XLit		LiteralFmt

	-- | A primitive function.
	| XPrim		Prim	Type

	-- | A variable with its type.
	| XVar		Var	Type

	-- | Type abstraction.
	| XLAM		Bind 	Kind	Exp

	-- | Type application.
	| XAPP		Exp	Type

	-- | Value abstraction.
	| XLam		Var	Type	Exp  Effect Closure

	-- | Value application.
	| XApp		Exp	Exp

	-- | Somee statements to execute in order.
	| XDo		[Stmt]

	-- | Matching and branching.
	| XMatch	[Alt]

	-- | Introduce a local region.
	| XLocal	Var	[(Var, Type)] Exp

	-- | A type annotation.
	| XTau		Type	Exp
	deriving (Show, Eq)


-- Stmt --------------------------------------------------------------------------------------------
-- | A statement or binding.
data Stmt
	= SBind  	(Maybe Var) Exp
	deriving (Show, Eq)


-- Alt ---------------------------------------------------------------------------------------------
-- | A pattern alternative.
data Alt
	= AAlt		[Guard] Exp
	deriving (Show, Eq)

-- | A pattern guard.
data Guard
	= GExp		Pat	Exp
	deriving (Show, Eq)

-- | A pattern.
data Pat
	-- | Bind a variable.
	= WVar	Var

	-- | Match against a literal value.
	| WLit	SourcePos LiteralFmt

	-- | Match against a constructor and bind its arguments.
	--   Not all the arguments need to be bound.
	| WCon	SourcePos
		Var 			-- constructor name
		[(Label, Var, Type)]	-- arguments.
	deriving (Show, Eq)


-- | A field label.
--   TODO: ditch the var constructor.
--   We should just do the var to index rewrite during conversion to core.
data Label
	= LIndex	Int		-- ^ i'th field of constructor.
	| LVar		Var		-- ^ a field name.
	deriving (Show, Eq)

