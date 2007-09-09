-----
-- Source.Exp
--
-- Summary:
--	Source expression tree data type.
--
--
module Source.Exp
	( Tree
	, Top 		(..)
	, Foreign 	(..)
	, DataField 	(..)
	, Annot 	(..)
	, InfixMode 	(..)
	, FixDef
	, Ctor
	, DataDef

	-- Value expressions
	, Exp 		(..)
	, Stmt 		(..), 	takeStmtBoundV
	, Proj		(..)
	, Alt 		(..)
	, Guard 	(..)
	, Pat 		(..)
	, Label		(..)

	, LCQual (..)
	, Var
	, takeVar

	-- Re-exported from Type.Exp
	, module Type.Exp
	
	-- Re-exported from Shared.Literal
	, Const   (..)
	, Literal (..))

where

-----
import Util

import qualified Shared.Var	as Var
import Shared.Base		(SourcePos)
import Shared.Var 		(Var, Module)
import Shared.Literal
import Shared.Exp
import Type.Exp			hiding (Var)

-----------------------
-- Top level expressions
--
type Tree	= [Top]

data Top
	= PPragma	[Exp]
	| PModule	Module
	| PType		SourcePos Var Type
	| PInfix	InfixMode Int [Var]

	-- Imports
	| PImportExtern	Var Type (Maybe Type)		-- name, value type, operational type
	| PImportModule [Module]

	| PForeign	Foreign

	-- Data 
	| PData	
		Var 
		[Var] 
		[(Var, [DataField Exp Type])]

	-- Effects
	| PEffect	Var Kind
	| PRegion	Var

	-- Classes
	| PClass	Var Kind			-- An abstract class.

	| PClassDict					-- A class dictionary definition.
		Var 					-- Class name.
		[Var] 					-- Class parameters
		[(Var, [Var])]  			-- Context.
		[([Var], Type)]				-- Type sigs.

	| PClassInst					-- A class instance.
		Var 					-- Class name
		[Type]					-- Instance types
		[(Var, [Type])]				-- Context.
		[Stmt]					-- Instance defs

	-- Projections
	| PProjDict					-- A projection dictionary.
		Type					-- Projection type.
		[Stmt]					-- Projection functions.

	-- Stmts
	| PStmt		Stmt
	deriving (Show, Eq)

-----
data Foreign
	= OImport Foreign 
	| OExport Foreign
	| OCCall  (Maybe String) Var Type
	| OExtern (Maybe String) Var Type (Maybe Type)
	deriving (Show, Eq)

-----
-- InfixMode
-- 	Infix operator associativity.
--
data InfixMode

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

type FixDef	= (Var, (Int, InfixMode))

type Ctor	= (Var, [DataField Exp Type])
type DataDef	= (Var, [Var], [Ctor])

type SP		= SourcePos

-----------------------
-- Value expressions
--
data Exp
	= XNil
	| XAnnot	[Annot]	Exp

	-- This is the core language that is checked by the type system.
	--
	| XUnit		SP
	| XVoid		SP				-- used in patterns to represent an unused element
	| XConst	SP Const			-- CONST

	| XVar 		SP Var				-- VAR
	| XProj		SP Exp Proj			-- EXP . PROJ
	
	| XLambda 	SP Var Exp			-- \VAR -> EXP
	| XApp 		SP Exp Exp			-- EXP1 EXP2

	| XCase 	SP Exp [Alt]			-- case EXP of { ALTS }

	| XLet		SP [Stmt] Exp			-- let STMTS in EXP
	| XDo		SP [Stmt]			-- do { STMTS }
	
	| XIfThenElse	SP Exp Exp Exp			-- if EXP1 then EXP2 else EXP3

	-- Type constraint slurper converts the previous forms
	--	these effect annotated forms.
	--
	| XAppE		SP Exp	Exp	[Effect]	-- effect caused by application.
	| XCaseE	SP Exp	[Alt]	[Effect]	-- effect caused by matching case object.

	-- The slurper binds fresh value variables to
	--	all nodes in a patter via XAt.
	| XAt		SP Var		Exp

	-- Syntactic sugar
	--	None of these make it to the type-constraint slurper.

	-- object expressions, desugared by Source.Rename
	| XObjVar	SP Var				-- ~var.  Binds current object.
	| XObjField	SP Var				-- _var.  A field of the current object.
	| XObjFieldR	SP Var				-- _#var. A reference to the field of the current object.

	-- infix expressions, desugared by Source.Defix
	| XOp		SP Var				-- An infix operator
	| XDefix	SP [Exp]			-- Some collection of apps / suspensions / infix expressions
	| XDefixApps	SP [Exp]
	| XAppSusp	SP Exp Exp			-- ex @ e1 .. eN	=> suspendN ex e1 .. eN

	-- lambda sugar, desugared by Source.Desugar
	| XLambdaPats	SP [Exp] Exp			-- \p1 p2 .. -> e
	| XLambdaProj 	SP Proj	[Exp]			-- \.field e1 e2 ...	=> \x' -> x'.field e1 e2 ...
	| XLambdaCase	SP [Alt]			-- \case { ALTS }	=> \x' -> case x' of { ALTS }

	-- match sugar, desugared by Source.Desugar
	| XMatch	SP [Alt]			-- match { ALTS };

	-- exception sugar, desugared by Source.Desugar
	| XTry		SP Exp [Alt] (Maybe Exp)	-- exp, catchAlts, with exp
	| XThrow	SP Exp	

	-- imperative sugar, desugared by Source.Desugar
	| XWhile	SP Exp Exp			-- test, body
	| XWhen		SP Exp Exp			-- test, body
	| XUnless	SP Exp Exp			-- test, body
	| XBreak	SP				--			=> throw ExceptionBreak

	-- list range sugar, desugared by Source.Desugar
	| XListRange	SP Bool Exp (Maybe Exp)		-- lazy, from, to.
	| XListComp	SP Exp [LCQual]			-- lazy, exp, qualifiers.

	-- data expressions/patterns sugar, desugared by Source.Desugar
	| XCon		SP Var [Exp]
	| XTuple	SP [Exp]
	| XCons		SP Exp Exp
	| XList		SP [Exp]
	
	deriving (Show, Eq)


-- | Projections
--	Used by XProjF
data Proj
	= JField	Var				-- Field projection, 		eg exp .field1
	| JFieldR	Var				-- Field reference projection,	eg exp #field1

	| JAttr		Var				-- Object attribute projection,	eg exp .{field1}

	| JIndex	Exp				-- Object index,		eg exp1.[exp2]
	| JIndexR	Exp				-- Object reference,		eg exp1#[exp2]
	deriving (Show, Eq)


-- | Annotations.
--	These appear in XAnnot.
data Annot
	= ATypeVar	Var				-- ^ A type variable for an Exp node.
	| AEffectVar	Var				-- ^ An effect variable for am Exp node.
	deriving (Show, Eq)


-- | Statements and bindings
--	Used by XLet and XDo.
data Stmt	
	= SBind		SP (Maybe Var) Exp		-- ^ A binding or stmt.
	| SSig		SP Var Type

	-- pattern sugar, desugared by Source.Hacks
	| SBindPats	SP Var [Exp] Exp

	deriving (Show, Eq)
	

-- | Case alternatives.
--	Used by XCase, XCaseL and XTry.
data Alt
	= APat		Pat 	Exp			-- ^ Case style pattern match	p1 -> e2
	| AAlt		[Guard]	Exp			-- ^ Match style pattern match  guards -> e2

	| ADefault	Exp				-- ^ Default alternative. 
							--	There should only be one of these per set of alts.

	deriving (Show, Eq)

data Guard
	= GCase		Pat				-- ^ Match against case object
	| GExp		Pat Exp				-- ^ Match against this expression.

	| GBool		Exp				-- ^ Test for boolean.
	| GBoolU	Exp				-- ^ Test for unboxed boolean.

	deriving (Show, Eq)
	
data Pat
	= WVar		Var				-- ^ Plain var, always matches.		v 
	| WConst	Const				-- ^ Constant pattern			5
	| WCon		Var [Pat]			-- ^ A constructor pattern		(C p1 p2 ...)
	| WConLabel	Var [(Label, Pat)]		-- ^ A constructor with field labels.	Con { .f1 = p1, ... }
	| WAt		Var Pat				-- ^ At expression			v@pat
	| WWildcard		 			-- ^ Wildcard, always matches		_

	| WUnit						-- ^ The unit value			()
	| WTuple	[Pat]				-- ^ Tuple pattern			(p1, p2)
	| WCons		Pat Pat				-- ^ Cons pattern			(x : xs)
	| WList		[Pat]				-- ^ List pattern			[p1, p2, ...]

	-- Contains an XDefix from the parser.
	--	The defixer is run on patterns as well as expressions
	| WExp		Exp
	deriving (Show, Eq)

data Label
	= LIndex 	Int				-- ^ A numerically indexed field.
	| LVar		Var				-- ^ A field label.
	deriving (Show, Eq)

-- |  List comprehension qualifiers
--	Used by XListComp.
data LCQual
	= LCGen		Bool Exp Exp			-- ^ Generator.			p <@- e, p <- e
	| LCLet		[Stmt]				-- ^ Local declaration.		Stmt can only be SBind.
	| LCExp		Exp				-- ^ Guard.
	deriving (Show, Eq)

takeVar e	= case e of { XVar sp v	-> Just v; _ -> Nothing; }

takeStmtBoundV s
 = case s of 
	SBind 	  sp v e	-> v
	SBindPats sp v  es x	-> Just v
	SSig      sp v  t	-> Just v	

