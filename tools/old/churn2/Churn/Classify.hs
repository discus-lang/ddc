module Churn.Classify
	( Expance (..)
	, Fragment (..)
	, allNonErrorFragments
	, classifyTableExp)
where

import Churn.Tag

import Source.Exp

import GHC.Base

-- | Whether or not a particular node of the AST is a terminal,
--   or contains branches. Also a measure of the minimum size of an expression.
--	
data Expance
	= EE	-- empty or errornous, like XNil
	| ET	-- terminal expressions, like constants or variables
	| ENT	-- non terminals	
	deriving (Show)

-- | What fragment of the language an expression belongs to
data Fragment
	-- Errornous things, like XNil.
	= FError

	-- Basic languge features.
	| FBase

	-- Thinly sugared versions of other things.
	| FSugar

	-- Projections.
	| FProject

	-- Object syntax.
	| FObject
	
	-- Imperative style loops / when / where
	| FImperative
	
	-- Exception mechanism
	| FException
	deriving (Show)

allNonErrorFragments
	= [FError, FBase, FSugar, FProject, FObject, FImperative, FException]

-- | Classification table for source expressions.
classifyTableExp :: [(Tag (Exp a), Expance, [Fragment])]
classifyTableExp
 = 	[ (tag XNil{},		EE,	[FError])
	, (tag XLit{},		ET,	[FBase])
	, (tag XVar{},		ET,	[FBase]) 
	, (tag XObjField{},	ET,	[FObject]) 
	, (tag XProj{},		ENT,	[FProject])
	, (tag XProjT{},	ET,	[FProject])
	, (tag XLambdaPats{},	ENT,	[FSugar])
	, (tag XLambdaProj{},	ET, 	[FSugar, FProject])
	, (tag XLambdaCase{},	ENT,	[FSugar, FProject])
	, (tag XCase{},		ENT,	[FSugar])
	, (tag XMatch{},	ENT,	[FBase])
	, (tag XDo{},		ENT,	[FBase])
	, (tag XLet{},		ENT,	[FSugar])
	, (tag XIfThenElse{},	ENT,	[FSugar])
	, (tag XTry{},		ENT,	[FException])
	, (tag XThrow{},	ENT,	[FException])
	, (tag XWhere{},	ENT,	[FSugar])
	, (tag XTuple{},	ENT,	[FSugar])
	, (tag XList{},		ENT,	[FSugar])
	, (tag XListRange{},	ENT,	[FSugar])
	, (tag XListComp{},	ENT,	[FSugar])
	, (tag XWhile{},	ENT,	[FImperative])
	, (tag XWhen{},		ENT,	[FImperative])
	, (tag XUnless{},	ENT,	[FImperative])
	, (tag XBreak{},	ENT,	[FException])
	]

