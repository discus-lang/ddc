
-- | Data type for source language.
module Source.Exp
	( Tree
	, Top 		(..)
	, Export	(..)
	, Foreign 	(..)
	, DataField 	(..)
	, InfixMode 	(..)
	, FixDef
	, Ctor
	, DataDef

	-- Value expressions
	, Exp 		(..)
	, Stmt 		(..)
	, Proj		(..)
	, Alt 		(..)
	, Guard 	(..)
	, Pat 		(..)
	, Label		(..)

	, LCQual (..)
	, Var

	-- Re-exported from Type.Exp
	, module Type.Exp
	
	-- Re-exported from Shared.Literal
	, Literal (..)
	, LiteralFmt (..))

where

import qualified Shared.Var	as Var
import Shared.Base		(SourcePos)
import Shared.Var 		(Var, Module)
import Shared.Literal
import Shared.Exp
import Type.Exp			hiding (Var)

import Util


-- Tree --------------------------------------------------------------------------------------------
type Tree a	= [Top a]


-- Top ---------------------------------------------------------------------------------------------
data Top a
	= PPragma	a [Exp a]
	| PModule	a Module
	| PInfix	a (InfixMode a) Int [Var]

	-- Imports
	| PImportModule a [Module]
	| PExport	a [Export a]

	| PForeign	a (Foreign a)

	-- Types
	| PTypeSynonym	a Var Type			-- Define a type synonym.
	| PTypeKind	a Var Kind			-- Define the kind of a type constructor.

	| PData	a					-- Define an algebraic data type.
		Var 
		[Var] 
		[(Var, [DataField (Exp a) Type])]

	-- Effects
	| PEffect a	Var Kind			-- Define the kind of an effect constructor.
	| PRegion a	Var				-- Introduce a top level region.

	-- Classes
	| PClass  a	Var Kind			-- An abstract class.

	| PClassDict					-- A class dictionary definition.
		a
		Var 					-- Class name.
		[(Var, Kind)] 				-- Class parameters
		[(Var, [Var])]  			-- Context.
		[([Var], Type)]				-- Type sigs.

	| PClassInst					-- A class instance.
		a
		Var 					-- Class name
		[Type]					-- Instance types
		[(Var, [Type])]				-- Context.
		[Stmt a]				-- Instance defs

	-- Projections
	| PProjDict					-- A projection dictionary.
		a
		Type					-- Projection type.
		[Stmt a]				-- Projection functions.

	-- Stmts
	| PStmt	(Stmt a)
	deriving (Show, Eq)


-- TODO: make these into their own data types.
type FixDef a	= (Var, (Int, InfixMode a))
type Ctor a	= (Var, [DataField (Exp a) Type])
type DataDef a	= (Var, [Var], [Ctor a])

-- Export ------------------------------------------------------------------------------------------
data Export a
	= EValue  a Var
	| EType   a Var
	| ERegion a Var
	| EEffect a Var
	| EClass  a Var
	deriving (Show, Eq)

-- Foreign -----------------------------------------------------------------------------------------
data Foreign a
	-- Import a value binding
	= OImport 
		(Maybe String) 	-- external name
		Var 		-- name of binding
		Type 		-- type of binding
		(Maybe Type)	-- operational type of binding
		
	-- Import an unboxed data type
	| OImportUnboxedData
		String		-- external name
		Var		-- name of data type
		Kind		-- kind of type
	deriving (Show, Eq)


-- InfixMode ---------------------------------------------------------------------------------------
data InfixMode a

	-- Left associative.
	--	x * y * z => * (* x y) z
	= InfixLeft					
	
	-- Right associative.
	--	x * y * z => * x (* y z)
	| InfixRight					

	-- Non associative.
	--	x * y * z => error
	| InfixNone					
	
	-- Magical Suspend operator associativity.
	--	Used internally in Source.Defix
	--
	--	f @ x y @ z => suspend1 (suspend2 f x y) z
	--
	| InfixSuspend					
	
	deriving (Show, Eq)


-- Value Expressions -------------------------------------------------------------------------------
data Exp a
	= XNil

	| XLit		a LiteralFmt			-- LIT
	| XVar 		a Var				-- VAR
	| XObjField	a Var				-- _VAR.
	| XProj		a (Exp a) (Proj a)		-- EXP . PROJ
	| XProjT	a Type (Proj a)			-- VAR & { TYPE }
	| XLambdaPats	a [Pat a]  (Exp a)		-- \p1 p2 .. -> e
	| XLambdaProj 	a (Proj a) [Exp a]		-- \.field e1 e2 ...	=> \x' -> x'.field e1 e2 ...
	| XLambdaCase	a [Alt a]			-- \case { ALTS }	=> \x' -> case x' of { ALTS }
	| XCase 	a (Exp a) [Alt a]		-- case EXP of { ALTS }
	| XMatch	a [Alt a]			-- match { ALTS };
	| XDo		a [Stmt a]			-- do { STMTS }
	| XLet		a [Stmt a] (Exp a)		-- let STMTS in EXP
	| XIfThenElse	a (Exp a) (Exp a) (Exp a)	-- if EXP1 then EXP2 else EXP3
	| XTry		a (Exp a) [Alt a] (Maybe (Exp a)) -- try EXP catch { ALTS } (with EXP)
	| XThrow	a (Exp a)	

	| XWhere	a (Exp a) [Stmt a]		-- EXP where { STMTS }

	| XTuple	a [Exp a]
	| XList		a [Exp a]

	| XListRange	a Bool (Exp a) (Maybe (Exp a)) (Maybe (Exp a))	-- [EXP .. EXP] / [EXP, EXP .. EXP] / [EXP .. ]
	| XListComp	a (Exp a) [LCQual a]		-- [ EXP | QUAL .. ]

	| XWhile	a (Exp a) (Exp a)		-- test, body
	| XWhen		a (Exp a) (Exp a)		-- test, body
	| XUnless	a (Exp a) (Exp a)		-- test, body
	| XBreak	a				--			=> throw ExceptionBreak

	-- Source.Defix desuaring ---------------------
	| XDefix	a [Exp a]			-- Some collection of apps / suspensions / infix expressions
	| XDefixApps	a [Exp a]
	| XOp		a Var				-- An infix operator
	
	| XApp 		a (Exp a) (Exp a)		-- EXP1 EXP2
	| XAppSusp	a (Exp a) (Exp a)		-- ex @ e1 .. eN	=> suspendN ex e1 .. eN

	-- Used by Source.Parser.Exp to track  EXP.(EXP) indexing
	| XParens	a (Exp a)
	deriving (Show, Eq)


-- Projections -------------------------------------------------------------------------------------
data Proj a
	= JField	a Var				-- Field projection, 		eg exp .field1
	| JFieldR	a Var				-- Field reference projection,	eg exp #field1

	| JIndex	a (Exp a)			-- Object index,		eg exp1.[exp2]
	| JIndexR	a (Exp a)			-- Object reference,		eg exp1#[exp2]
	deriving (Show, Eq)


-- Statements --------------------------------------------------------------------------------------
data Stmt a	
	= SSig		a [Var] Type
	| SStmt		a (Exp a)			-- ^ a statement (with no arguments)
	
	| SBindFun	a Var [Pat a] [Alt a]		-- ^ a guarded function binding, with patterns for the arguments. 

	| SBindPat	a (Pat a) (Exp a)		-- ^ an irrefutable pattern binding
	| SBindMonadic	a (Pat a) (Exp a)		-- ^ a monadic bind. Desugars to  (exp `bind` \var -> ...)
	deriving (Show, Eq)
	

-- Alternatives / Guards / Patterns ----------------------------------------------------------------
data Alt a
	= APat		a (Pat a) (Exp a)		-- ^ Case style pattern match	p1 -> e2
	| AAlt		a [Guard a] (Exp a)		-- ^ Match style pattern match  guards -> e2

	| ADefault	a (Exp a)			-- ^ Default alternative. 
							--	There should only be one of these per set of alts.

	deriving (Show, Eq)

data Guard a
	= GExp		a (Pat a) (Exp a)		-- ^ Match against this expression.
	| GBool		a (Exp a)			-- ^ Test for boolean.
	deriving (Show, Eq)
	
data Pat a
	= WVar		a Var				-- ^ Plain var, always matches.		v 
	| WObjVar	a Var				-- ^ Binds the current object.		^v
	| WLit		a LiteralFmt			-- ^ Match a literal value		5
	| WCon		a Var [Pat a]			-- ^ A constructor pattern		(C p1 p2 ...)
	| WConLabel	a Var [(Label a, Pat a)]	-- ^ A constructor with field labels.	Con { .f1 = p1, ... }
	| WAt		a Var (Pat a)			-- ^ At expression			v\@pat
	| WWildcard	a	 			-- ^ Wildcard, always matches		_

	| WUnit		a				-- ^ The unit value			()
	| WTuple	a [Pat a]			-- ^ Tuple pattern			(p1, p2)
	| WCons		a (Pat a) (Pat a)		-- ^ Cons pattern			(x : xs)
	| WList		a [Pat a]			-- ^ List pattern			[p1, p2, ...]
	deriving (Show, Eq)

data Label a
	= LIndex 	a Int				-- ^ A numerically indexed field.
	| LVar		a Var				-- ^ A field label.
	deriving (Show, Eq)


-- List Comprehensions -----------------------------------------------------------------------------
data LCQual a
	= LCGen		Bool (Pat a) (Exp a)		-- ^ Generator.			p <\@- e, p <- e
	| LCLet		[Stmt a]			-- ^ Local declaration.		Stmt can only be SBind.
	| LCExp		(Exp a)				-- ^ Guard.
	deriving (Show, Eq)
