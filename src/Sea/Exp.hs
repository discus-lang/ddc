
-- | Abstract C expressions.
--	TODO: 	This is a mess. 
--		Most of these types have too many constructors that do basically
--		the same thing. We should try and reduce the size of these types.
module Sea.Exp
	( Tree
	, Top		(..)
	, CtorDef	(..)
	, Stmt		(..)
	, Alt		(..)
	, Guard		(..)
	, Exp		(..)
	, Type		(..)
	, ObjType	(..)
	, Prim		(..)
	, Var)
where
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
		Var				-- 	Type constructor.
		(Map Var CtorDef)

	-- supers ---------------------
	-- | A C prototype.
	| PProto
		Var 				--	variable name
		[Type] 				--	argument types
		Type				--	result type

	-- | Code for a supercombinator
	| PSuper
		Var				--	variable name
		[(Var, Type)] 			--	parameter names and types
		Type 				--	result type
		[Stmt a]			--	statements

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

	| PComment	String
	| PBlank
	deriving (Show, Eq)


-- Meta-data about a constructor.
--	Note that we need to remember the indicies of each field so we can convert
--	pattern matches using labels to Sea form. 
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
	= SBlank				-- a blank line, makes output code easier to read
	| SComment	String			-- a comment, 	 mostly used for debugging.

	-- stack
	| SAuto		Var Type		-- Define an automatic var.		Type var;
	| SEnter	Int 			-- Function entry code,	countS
	| SLeave	Int			-- Function exit code,	countS

	-- assignment
	| SAssign	(Exp a) Type (Exp a)	-- An assigment, 			x1 = x2.
	| SStmt		(Exp a)			-- Exp must be a XCall or an XApply

	-- control flow
	| SLabel	Var			-- Label for goto.			var:
	| SGoto		Var			-- Goto some label.			goto var;

	| SReturn	(Exp a)			-- Return from super.			return exp;

	| SMatch	[Alt a]

	| SIf		(Exp a) [Stmt a]

	| SSwitch	(Exp a) [Alt a]		-- Switch on an expression.
	| SCaseFail		
	deriving (Show, Eq)	


-- | A case or switch alternative.
data Alt a
	= AAlt		[Guard a] [Stmt a]	-- If all the guards succeed then run the stmts.
	| ASwitch	(Exp a)   [Stmt a]

	| ACaseSusp	(Exp a) Var		-- _CASESUSP (exp, label);
						--	// if exp is a susp, force and jump to label

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
		Var 				-- the name of the var it's currently holding
		Type				-- the type of the var
		Int				-- the index of the slot

	-- a reference to a CAF object ptr.
	--	This references a global variable refering to a CAF object.
	| XSlotCAF	Var Type


	-- application
	| XTailCall	Var [Exp a]
	| XCall		Var [Exp a]
	| XCallApp	Var Int [Exp a]
	| XApply	(Exp a) [Exp a]
	| XCurry	Var Int [Exp a]		-- super name, super airity, args
	
	| XSuspend	Var  [Exp a]		-- thunk name, args
	| XPrim		Prim [Exp a]

	-- projection
	| XArg		(Exp a) ObjType Int	-- of some object
	| XTag		(Exp a)			-- tag of data object	((Data)x) ->tag
	
	| XField	(Exp a) Var Var		-- exp, type of exp, field name
	| XFieldR	(Exp a) Var Var		-- exp, type of exp, field name
	
	-- constants
	| XCon		Var			-- a data constructor
	| XInt		Int			-- an integer
	| XUnit					-- the unit data type
	| XLit		LiteralFmt		-- a literal
	| XSuper	Var			-- name of a supercombinator
	| XAtom		Var

	-- control
	| XLabel	Var			-- a label, for jumping to
	| XTagThunk
	| XNull
	
	-- boxing
	| XBox		Type (Exp a)		-- type, exp
	| XUnbox	Type (Exp a)		-- type, exp
	| XForce	(Exp a)

	-- allocation
	| XAlloc		Int		-- heap allocation: size
	| XAllocThunk		Var Int Int	-- alloc thunk:	function name, airity, args in this thunk.
	| XAllocSusp		Var Int

	| XAllocData		Var Int		-- alloc data:  ctor name, airity
	| XAllocDataAnchored	Var Int
	
	deriving (Show, Eq)


-- | Sea types. 
--	By the time we've reached the Sea language we only care about operational information. 
--	We need to distinguish between boxed and unboxed values, but not much else.
data Type
	-- | The void type.
	= TVoid

	-- | The function type.
	--	These are always first order, so the first parameter will not be another TFun.
	| TFun Type Type

	-- | An unboxed pointer to something else.
	| TPtr Type
	
	-- | An unboxed data object.
	| TCon Var [Type]

	-- | Some anonymous boxed object. Usually accessed as a pointer.
	| TObj
	deriving (Show, Eq)


-- | When we access fields in an object we need to know exactly what type
--	we are dealing with.
data ObjType
	= TObjData
	| TObjThunk
	| TObjSusp
	deriving (Show, Eq)


-- | Primitive operators implemented directly in the C language or runtime system.
--	We keep these separate from the Core Op type because the two languages
--	might implement different operators.
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

