
module Desugar.Exp
(
	Tree,
	Top		(..),
	CtorDef		(..),
	ClassContext	(..),
	Exp		(..),
	Proj		(..),
	Stmt		(..),
	Alt		(..),
	Guard		(..),
	Pat		(..),
	Label		(..)
)

where

import	Util
import	Shared.Var	(Var, Module)
import	Shared.Literal
import 	Shared.Exp	(DataField)
import	Type.Exp

-----
--	This is an annotated version of the subset of the source language
--	which the type inferencer deals with.
--
type Tree a	= [Top a]

data Top a
	= PNil

	-- imports
	| PImport	a [Module]
	| PExtern	a Var Type (Maybe Type)

	-- effect / region defs
	| PEffect	a Var Kind
	| PRegion	a Var

	-- data defs
	| PData		a Var [Var]  [CtorDef a]

	-- classes
	| PClass	a Var Kind
	| PClassDict	a Var [Type] [ClassContext] [(Var, Type)]
	| PClassInst	a Var [Type] [ClassContext] [(Var, Exp a)]

	-- projections
	| PProjDict	a Type [Stmt a]

	-- sigs
	| PSig		a Var Type

	-- bindings
	| PBind 	a (Maybe Var) (Exp a)
	deriving (Show, Eq)

data CtorDef a
	= CtorDef a Var [DataField (Exp a) Type]
	deriving (Show, Eq)

data ClassContext 
	= ClassContext Var [Type]
	deriving (Show, Eq)

data Exp a
	= XNil

	-- Accepted by the constraint slurper.
	| XVoid	 	a
	| XConst 	a Const
	| XVar	 	a Var
	| XProj		a (Exp a)  (Proj a)		
	| XProjT	a Type (Proj a)		
	| XLambda	a Var (Exp a) 
	| XApp		a (Exp a) (Exp a)
	| XMatch     	a (Maybe (Exp a)) [Alt a]
	| XDo       	a [Stmt a]
	| XIfThenElse	a (Exp a) (Exp a) (Exp a)

	-- Produced by the constraint slurper
	| XLambdaTEC 	a Var (Exp a) Type Effect Closure
	| XProjTagged	a Var (Exp a) (Proj a)
	| XProjTaggedT  a Var (Proj a)
	| XVarInst	a Var				-- An instance of a let bound variable
							--	We'll need to add TREC applications to this variable
							--	during Desugar->Core translation.

	deriving (Show, Eq)
	
	
data Proj a
	= JField 	a Var
	| JFieldR	a Var
	deriving (Show, Eq)
	

data Stmt a
	= SBind 	a (Maybe Var) (Exp a)
	| SSig		a Var	  Type
	deriving (Show, Eq)
	

data Alt a
	= AAlt 		a [Guard a]	(Exp a)
	deriving (Show, Eq)
	
data Guard a
	= GCase		a (Pat a)
	| GExp		a (Pat a)	(Exp a)
	deriving (Show, Eq)
	
data Pat a
	= WConLabel	a Var [(Label a, Var)]
	| WConst	a Const

	-- Eliminated by Desugar.Patterns
	| WVar		a Var
	| WAt		a Var (Pat a)
	| WConLabelP	a Var [(Label a, Pat a)]
	| WWildcard	a
	deriving (Show, Eq)

data Label a
	= LIndex	a Int
	| LVar		a Var
	deriving (Show, Eq)


