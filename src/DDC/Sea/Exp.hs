{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Abstract C expressions.
module DDC.Sea.Exp
	( module DDC.Sea.Exp.Prim
	, module DDC.Sea.Exp.Type
	, module DDC.Sea.Exp.Name
	, Tree
	, Top		(..)
	, CtorDef	(..)
	, Stmt		(..)
	, Alt		(..)
	, Guard		(..)
	, Exp		(..)
	, Lit		(..)
	, Var)
where
import DDC.Sea.Exp.Prim
import DDC.Sea.Exp.Type
import DDC.Sea.Exp.Name
import DDC.Base.SourcePos
import DDC.Base.Literal
import DDC.Var
import Util


-- | A whole C program.
type Tree a
	= [Top a]


-- | Top level definitions.
data Top a
	-- | A missing thing \/ hole (used for debugging).
	= PNil

	-- | Inject a comment into the Sea file.
	| PComment	String

	-- | Inject a blank line into the Sea file, to make it easier to read.
	| PBlank

	-- | Data or functions defined external to this module.
	| PExtern
		Var				-- Name of external.
		Type				-- Its Type.

	-- | Data type definition.
	| PData
		Var				--  Type constructor name.
		(Map Var CtorDef)		--  Data constructors.

	-- | A C prototype.
	| PProto
		Var 				--  Function name.
		[Type] 				--  Argument types
		Type				--  Result Type.

	-- | Code for a supercombinator
	| PSuper
		Var				-- variable name
		[(Var, Type)] 			-- parameter names and types
		Type 				-- result type
		[Stmt a]			-- statements

	-- | Constructor struct definition (for constructors with unboxed fields).
	| PCtorStruct
		Var				-- struct name
		[(Int, Type)]			-- field index and types

	-- | Constructor tag name and value
	| PCtorTag	String Int		-- ^ Constructor name and value.

	-- cafs -----------------------
	| PCafProto	Var Type		-- ^ The prototype for a CAF slot index.
	| PCafSlot	Var Type		-- ^ A var which holds a CAF slot index.
	| PCafInit	Var Type [Stmt a]	-- ^ CAF initialisation code.


	-- | The program entry point.
	| PMain
		{ -- | Name of the module containis the C main function.
		  topMainModuleName	:: String

		  -- | Names of modules imported by the main module.
		, topMainImported	:: [String]

		  -- | Whether to wrap the Disciple main fn in the top-level
		  --   exception handler from the prelude.
		  --   TODO: Handle this in the desugarer in instead.
		, topDefaultHandler	:: Bool

		  -- | Starting size of heap,          or Nothing for default.
		, topStartHeapSize		:: Maybe Integer

		  -- | Starting size of slot stack,    or Nothing for default.
		, topStartSlotStackSize		:: Maybe Integer

		  -- | STarting size of context stack, or Nothing for default.
		, topStartContextStackSize	:: Maybe Integer }

	deriving (Show, Eq)


-- | Meta-data about a constructor.
--   Note that we need to remember the indicies of each field so we can convert
---  pattern matches using labels to Sea form.
--
data CtorDef
	= CtorDef
	{ ctorDefName 		:: Var 		-- ^ name of constructor
	, ctorDefType		:: Type		-- ^ type of constructor
	, ctorDefArity		:: Int		-- ^ arity of constructor (number of params)
	, ctorDefTag		:: Int		-- ^ tag of constructor   (order in original data type decl)
	, ctorDefFields		:: Map Var Int	-- ^ map of field names to indexes in the constructor.
	, ctorDefFieldTypes	:: [Type] }	-- ^ map of field names to indexes in the constructor.
	deriving (Show, Eq)


-- | An Abstract C statement.
data Stmt a
	-- misc
	= SBlank				-- ^ a blank line, makes output code easier to read
	| SComment	String			-- ^ a comment, mostly used for debugging.

	-- stack
	| SAuto		Var Type		-- Define an automatic var.
	| SEnter	Int 			-- Expands to function entry code, pushing slots onto the stack.
	| SLeave	Int			-- Expands to function exit code,  popping slots from the stack.

	-- assignment
	| SAssign	(Exp a) Type (Exp a)	-- An assigment.
	| SStmt		(Exp a)			-- Exp must be a XCall or an XApply

	-- control flow
	| SLabel	Var			-- Label for goto
	| SGoto		Var			-- Goto some label

	| SReturn	(Exp a)			-- Return from super

	| SMatch	[Alt a]			-- Pattern matching

	| SIf		(Exp a) [Stmt a]	-- If-then-else expression.

	| SSwitch	(Exp a) [Alt a]		-- Switch on an expression.
	| SCaseFail	SourcePos
	deriving (Show, Eq)


-- | A case or switch alternative.
data Alt a
	= AAlt		[Guard a] [Stmt a]	-- If all the guards succeed then run the stmts.
	| ASwitch	(Exp a)   [Stmt a]

	| ACaseSusp	(Exp a) Var		-- _CASESUSP (exp, label);
						--	// if exp is a susp, force and jump to label

	| ACaseIndir	(Exp a) Var		-- _CASEINDIR (exp, label);
						--	// if exp is an indirection, follow it and jump to label

	| ACaseDeath 	SourcePos		-- _CASEDEATH (file, line, column);

	| ADefault	[Stmt a]
	deriving (Show, Eq)


-- | A case guard.
data Guard a
	-- Run some stmts then check if two objects have the same tag
	= GCase					-- a guard in a case expression
			SourcePos		-- source filename, line and column
			Bool			-- true if the case object is in a lazy region
			[Stmt a] 		-- run these statements
			(Exp a)			-- check if this value (the case object)
			(Exp a)			-- matches this one (always a var)
	deriving (Show, Eq)


-- | Expressions
data Exp a
	-- | A missing node in the AST. Used for debugging.
	= XNil

	-- | Value variables with their types.
	| XVar		Name Type

	-- | Literal values.
	| XLit		Lit

	-- | Take a numbered field from some boxed data object.
	| XArgBoxedData	(Exp a) Int

	-- | Take a numbered field from some unboxed data object.
	| XArgUnboxedData	(Exp a) Int

	-- | Take a numbered field from some thunk.
	| XArgThunk	(Exp a) Int

	-- | Take the tag of a boxed object.
	| XTag		(Exp a)

	-- | Invoke a primitive operator.
	| XPrim		Prim [Exp a]
	deriving (Show, Eq)


-- | A literal value.
data Lit
	-- | The null pointer.
	= LNull

	-- | The boxed unit value.
	| LUnit

	-- | The tag of a data constructor.
	| LDataTag	Var

	-- | A literal of some primitve type like int or float.
	| LLit		LiteralFmt
	deriving (Show, Eq)
