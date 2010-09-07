{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Abstract C expressions.
--   TODO: This is a mess.
--	   Most of these types have too many constructors that do basically
--	   the same thing. We should try and reduce the size of these types.
module DDC.Sea.Exp
	( module DDC.Sea.Exp.Prim
	, module DDC.Sea.Exp.Type
	, Tree
	, Top		(..)
	, CtorDef	(..)
	, Stmt		(..)
	, Alt		(..)
	, Guard		(..)
	, Exp		(..)
	, Var)
where
import DDC.Sea.Exp.Prim
import DDC.Sea.Exp.Type
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


	-- cafs -----------------------
	| PCafProto	Var Type		-- ^ The prototype for a CAF slot index.
	| PCafSlot	Var Type		-- ^ A var which holds a CAF slot index.
	| PCafInit	Var Type [Stmt a]	-- ^ CAF initialisation code.


	-- hackery --------------------
	-- various hacky things that should probably be handled in a different way.
	| PHashDef	String String		-- evil hash-def. Used to define constructor tags.
	| PInclude	String			-- #include <...>
	| PIncludeAbs	String			-- #include "..."

	| PHackery	String			-- string is inlined straight into the output file
	| PMain		String [String]		-- C main() function with moduleName and imported modules

	| PComment	String
	| PBlank
	deriving (Show, Eq)


-- | Meta-data about a constructor.
--   Note that we need to remember the indicies of each field so we can convert
---  pattern matches using labels to Sea form.
--
data CtorDef
	= CtorDef
	{ ctorDefName 	:: Var 			-- ^ name of constructor
	, ctorDefType	:: Type			-- ^ type of constructor
	, ctorDefArity	:: Int			-- ^ arity of constructor (number of params)
	, ctorDefTag	:: Int			-- ^ tag of constructor   (order in original data type decl)
	, ctorDefFields	:: Map Var Int }	-- ^ map of field names to indexes in the constructor.
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
	| SCaseFail
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
--   TODO: There are still too many constructors in this type.
--	   We probably want to make an LValue/RValue split.
data Exp a
	= XNil

	-- Assignable names.
	-- TODO: merge these into the same constructor.
	| XVar		Var Type
	| XVarCAF	Var Type

	-- | A slot on the GC stack.
	--	All pointers to objects in the heap must be on the slot stack
	--	when we do something that might cause a garbage collection.
	| XSlot
		Var 				-- the name of the var it's currently holding
		Type				-- the type of the var
		Int				-- the index of the slot

	-- | A reference to a CAF object ptr.
	--	This references a global variable refering to a CAF object.
	| XSlotCAF	Var Type


	-- Literals
	-- TODO: break these out into their own type.
	| XNull					-- The null pointer.
	| XInt		Int			-- An integer.
	| XUnit					-- A unit value.
	| XLit		LiteralFmt		-- a literal
	| XSuper	Var			-- name of a supercombinator
	| XCon		Var			-- a data constructor
	| XTagThunk

	-- control
	| XLabel	Var			-- a label, for jumping to

	-- projection
	| XArg		(Exp a) ObjType Int	-- of some object
	| XTag		(Exp a)			-- tag of data object	((Data)x) ->tag

	-- invoke some primitive operator.
	| XPrim		Prim [Exp a]
	deriving (Show, Eq)



