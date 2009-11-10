
module Sea.Exp
	( Tree
	, Top		(..)
	, DataField 	(..)
	, Stmt		(..)
	, Alt		(..)
	, Guard		(..)
	, Exp		(..)
	, Type		(..)
	, Prim		(..)
	, Var)
where

import Util
import Shared.Base
import qualified Shared.Var	as Var
import Shared.Var		(Var)
import Shared.Exp
import Shared.Literal

type Tree a
	= [Top a]


data Top a

	---------------
	-- Meta
	--	These constructors don't represent nodes
	--	that make it into the actual C output.
	--

	= PNil

	| PData						--  Data type definition
		Var					-- 	Type name
		[(Var, [DataField Var Type])]		-- 	[(ctor name, datafield)]

	| PCtor						--  A Constructor.
		Var 					--	name
		[Type] 					--	arg types
		Type					-- 	result type

	---------------
	-- Sea	
	--	Each of these has a concrete representation
	--	in the C output.
	--

	-- supers
	| PProto	Var [Type] Type			-- super prototype. name, argTypes, resultType

	| PSuper					--  A Supercombinator
		Var					--	name
		[(Var, Type)] 				--	[(argName, argType)]
		Type 					--	result type
		[Stmt a]				--	expression

	-- cafs
	| PCafProto	Var Type			-- ^ The prototype for a CAF slot index.
	| PCafSlot	Var Type			-- ^ A var which holds a CAF slot index.
	| PCafInit	Var Type [Stmt a]		-- ^ CAF initialisation code.

	-- atoms
	| PAtomProto 	Var Type			-- ^ Atom prototype.
	| PAtom	 	Var Type			-- ^ Atom definition.

	| PStruct					--  Structure definition
		Var					-- 	type name
		[(Var, Type)]				-- 	(label, type)


	-- hackery
	| PHashDef	String String			-- evil hash-def. Used to define ctor tags.
	| PInclude	String				-- #include <...>
	| PIncludeAbs	String				-- #include "..."

	| PHackery	String				-- string is inlined straight into the output file

	| PComment	String
	| PBlank
	deriving (Show, Eq)


data Stmt a
	-- misc
	= SBlank					-- a blank line, makes output code easier to read
	| SComment	String				-- a comment, 	 mostly used for debugging.

	| SHackery	String				-- Some low level hackery inlined straight into the output file.
							-- 	this should only really be used for experimental purposes.

	-- stack
	| SAuto		Var Type			-- Define an automatic var.		Type var;
	| SEnter	Int 				-- Function entry code,	countS
	| SLeave	Int				-- Function exit code,	countS

	-- assignment
	| SAssign	(Exp a) Type (Exp a)		-- An assigment, 			x1 = x2.
	| SStmt		(Exp a)				-- Exp must be a XCall or an XApply

	-- control flow
	| SLabel	Var				-- Label for goto.			var:
	| SGoto		Var				-- Goto some label.			goto var;

	| SReturn	(Exp a)				-- Return from super.			return exp;

	| SMatch	[Alt a]

	| SIf		(Exp a) [Stmt a]

	| SSwitch	(Exp a) 	[Alt a]		-- Switch on an expression.
	| SCaseFail		
	deriving (Show, Eq)	


data Alt a
	= AAlt		[Guard a]	[Stmt a]	-- If all the guards succeed then run the stmts.
	| ASwitch	(Exp a) 	[Stmt a]

	| ACaseSusp	(Exp a) Var			-- _CASESUSP (exp, label);
							--	// if exp is a susp, force and jump to label

	| ACaseDeath 	SourcePos			-- _CASEDEATH (file, line, column);

	| ADefault	[Stmt a]
	deriving (Show, Eq)


data Guard a
	-- Run some stmts then check if two objects have the same tag
	= GCase						-- a guard in a case expression
			SourcePos			-- source filename, line and column
			Bool				-- true if the case object is in a lazy region
			[Stmt a] 			-- run these statements
			(Exp a)				-- check if this value (the case object)
			(Exp a)				-- matches this one (always a var)
	deriving (Show, Eq)
	

-- | Expressions 
--	TODO: There are way to many constructors in this type.
--	It'd be better to merge groups of similar ones, most of the application forms
--	could be merged, while using an auxillary data type to record what sort of application it is.
data Exp a
	= XNil

	-- Var-ish things
	| XVar		Var Type
	| XVarCAF	Var Type

	-- A slot on the GC stack.
	--	All pointers to objects in the heap must be on the slot stack
	--	when we do something that might cause a garbage collection.
	| XSlot		
		Var 			-- the name of the var it's currently holding
		Type			-- the type of the var
		Int			-- the index of the slot

	-- a reference to a CAF object ptr.
	--	This references a global variable refering to a CAF object.
	| XSlotCAF	Var Type


	-- application
	| XTailCall	Var [Exp a]
	| XCall		Var [Exp a]
	| XCallApp	Var Int [Exp a]
	| XApply	(Exp a) [Exp a]
	| XCurry	Var Int [Exp a]			-- super name, super airity, args
	
	| XSuspend	Var  [Exp a]			-- thunk name, args
	| XPrim		Prim [Exp a]

	-- projection
	| XArg		(Exp a) Type Int		-- argument of thunk	((type)x) ->a[i]
	| XTag		(Exp a)				-- tag of data object	((Data)x) ->tag
	
	| XField	(Exp a) Var Var			-- exp, type of exp, field name
	| XFieldR	(Exp a) Var Var			-- exp, type of exp, field name
	
	-- constants
	| XCon		Var				-- a data constructor
	| XInt		Int				-- an integer
	| XUnit						-- the unit data type
	| XLit		LiteralFmt			-- a literal
	| XSuper	Var				-- name of a supercombinator
	| XAtom		Var

	-- control
	| XLabel	Var				-- a label, for jumping to
	| XTagThunk
	| XNull
	
	-- boxing
	| XBox		Type (Exp a)			-- type, exp
	| XUnbox	Type (Exp a)			-- type, exp
	| XForce	(Exp a)

	-- allocation
	| XAlloc		Int			-- heap allocation: size
	| XAllocThunk		Var Int Int		-- alloc thunk:	function name, airity, args in this thunk.
	| XAllocSusp		Var Int

	| XAllocData		Var Int			-- alloc data:  ctor name, airity
	| XAllocDataAnchored	Var Int
	
	deriving (Show, Eq)
	
data Type
	-- | The void type.
	= TVoid

	-- | An unboxed pointer to something else.
	| TPtr Type
	
	-- | An unboxed data object.
	| TCon Var [Type]

	-- | A pointer to boxed object
	| TObj
	
	-- | A pointer to a boxed object, but we know a bit more about
	--	what that object actually is - usually because we just created it.
	--	These are subtypes of TObj
	| TData	
	| TThunk		
	| TSusp
	deriving (Show, Eq)


data	Prim
	= FNeg
	| FAdd
	| FSub
	| FMul
	| FDiv
	| FMod
	
	| FEq
	| FNEq

	| FGt
	| FLt
	
	| FGe
	| FLe
	
	| FAnd
	| FOr
	
	| FProjField
	| FProjFieldR
	
	| FArrayPeek Type
	| FArrayPoke Type
	
	| FStrCmp
	deriving (Show, Eq)

