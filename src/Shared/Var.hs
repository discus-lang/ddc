{-# OPTIONS -O2 #-}

module Shared.Var
	( module Shared.VarBind
	, module DDC.Base.NameSpace

	, Var (..)
	, new
	, (=~=)
	, (=^=)
	, loadSpaceQualifier

	, VarInfo(..)
	, Module(..) )

where

import Data.Char

import Shared.Pretty
import Shared.Literal
import Shared.Base
import Shared.VarBind
import Shared.Error

import DDC.Base.NameSpace

import Util

-----
stage	= "Shared.Var"

-----
data Var =
	Var 
	{ name		:: !String		-- ^ Name of this var.
	, nameModule	:: !Module		-- ^ The module path that this var was defined in.
	, nameSpace	:: !NameSpace
	, bind		:: !VarBind		-- ^ A unique identifier for this binding occurance.
	, info		:: ![VarInfo] }		-- ^ some (optional) info about this var.
	deriving Show

-----
new ::	String -> Var
new	n	
	= Var 
	{ name 		= n
	, nameModule	= ModuleNil
	, nameSpace	= NameNothing	
	, bind		= XNil
	, info		= [] }


-----
-- Comparisons
--
instance Eq Var where
  (==) v1 v2	= (=^=) v1 v2

instance Ord Var where
 compare v1 v2 	
  = case compare (bind v1) (bind v2) of
	EQ	-> compare (nameModule v1) (nameModule v2)
	ord	-> ord


-- | Compare by name
infix 4 =~=
(=~=) :: Var -> Var	-> Bool
(=~=) a b	= name a == name b

-- | Compare by binder
(=^=) :: Var -> Var	-> Bool
(=^=) a b	
	=   bind a == bind b
	&&  nameModule a == nameModule b


-- | Slurp namespace qualifiers from the var name into 
--	the space field.
loadSpaceQualifier :: Var -> Var
loadSpaceQualifier var
 = case takeHead (name var) of
	Just '%'	-> var { nameSpace = NameRegion,  name = tail (name var) }
	Just '!'	-> var { nameSpace = NameEffect,  name = tail (name var) }
	Just '$'	-> var { nameSpace = NameClosure, name = tail (name var) }
	_		-> var


-- Pretty print a variable
instance Pretty Var PMode where
 ppr v
  = case nameModule v of
	ModuleNil		-> ppr $ pprVarSpaced v
	ModuleAbsolute ns	-> 	 punc "." ((map ppr ns) ++ [pprVarSpaced v])
	ModuleRelative ns	-> "." % punc "." ((map ppr ns) ++ [pprVarSpaced v])
 
pprVarSpaced v
  = case nameSpace v of
	-- if no namespace print ?? as a warning
	NameNothing	-> "??" % pprVarName v

  	NameValue
	 -> case name v of
	  	(v1:_)
 	 	 | not $ isAlpha v1	-> parens $ pprVarName v
		 | otherwise		-> pprVarName v

		[] -> panic stage $ "prettyVarN: null var " % show v

	NameType
	 -> case name v of
	  	(v1:_)
 	 	 | not $ isAlpha v1	
		 -> ifMode (elem PrettyTypeSpaces)
		 	("*" % (parens $ pprVarName v))
			(parens $ pprVarName v)

		 | otherwise		
		 -> ifMode (elem PrettyTypeSpaces)
		 	("*" % pprVarName v)
			(pprVarName v)

		[] -> panic stage $ "prettyVarN: null var " % show v

	NameRegion	-> "%" % pprVarName v
	NameEffect	-> "!" % pprVarName v
	NameClosure	-> "$" % pprVarName v

	-- Classes/witness,
	--	Prepend a + to witness variables, these only show up in the core.
	--	
	NameClass	
	 -> let	(x:_)	= name v
	    in	if isUpper x 
	   		then pprVarName v
	   		else "+" % pprVarName v

	_ 		-> pprVarName v	 

pprVarName v
 = ifMode (elem PrettyUnique)	-- append unique binds if requested
 	(name v % "_" % bind v)
	(ppr $ name v)



-----------------------
-- VarInfo
--
data VarInfo
	= ISourcePos	SourcePos		-- ^ Where this var appears in the source.

	| IBoundBy	Var			-- ^ The binding occurance of this var.
	| ISchemeVar	Var			-- ^ If this var was instantiated, the type scheme var it came from.

						--  Only relavent if the var is a type var.
	| IValueVar	Var			-- ^	The value var this type var corresponds to.
	| IValueLiteral	Literal			-- ^	The literal value this type var represents.
	| IParent	Var			-- ^	??

	| IAlias	Var			-- ^ Indicates that this var is an alias/specialisation/instance 
						--	of some other var.

	| ISeaName	String			-- ^ Variable name to use when outputting Sea code.

	| IString	String			-- ^ Some string, for debugging

	deriving (Show)


instance Pretty VarInfo PMode where
 ppr x 	= ppr $ show x


-----------------------
data Module
	= ModuleNil				-- ^ No module information
	| ModuleAbsolute [String]		-- ^ Absolute module name, of the form "M1.M2.M3 ..."
	| ModuleRelative [String]		-- ^ A module name relative to the current one, of the form ".M1.M2.M3 ..."
	deriving (Show, Eq, Ord)

instance Pretty Module PMode where
 ppr m
  = case m of
	ModuleNil		-> ppr "@ModuleNil"
  	ModuleAbsolute vs	-> "." %!% vs
	ModuleRelative vs	-> "." % "." %!% vs


