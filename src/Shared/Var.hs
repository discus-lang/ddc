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
	, Module(..) 
	, noModule )
where
import Shared.Pretty
import Shared.Literal
import Shared.Base
import Shared.VarBind
import Shared.Error

import DDC.Base.NameSpace
import Data.Char
import Util

stage	= "Shared.Var"

-- Variables ------------------------------------------------------------------
data Var =
	Var 
	{ name		:: !String		-- ^ Name of this var.
	, nameModule	:: !Module		-- ^ The module path that this var was defined in.
	, nameSpace	:: !NameSpace
	, bind		:: !VarBind		-- ^ A unique identifier for this binding occurance.
	, info		:: ![VarInfo] }		-- ^ some (optional) info about this var.
	deriving Show

-- | Create a new variable with this name.
--	The unique binder is set to XNil (empty).
--
new ::	String -> Var
new	n	
	= Var 
	{ name 		= n
	, nameModule	= ModuleNil
	, nameSpace	= NameNothing	
	, bind		= XNil
	, info		= [] }


-- | Comparing variables for equality
instance Eq Var where
  (==) v1 v2	= (=^=) v1 v2


-- | Ordering of variables
instance Ord Var where
 compare v1 v2 	
  = case compare (bind v1) (bind v2) of
	EQ	-> compare (nameModule v1) (nameModule v2)
	ord	-> ord


-- | Compare variables just by their textual names
infix 4 =~=
(=~=) :: Var -> Var	-> Bool
(=~=) a b	= name a == name b


-- | Compare variables by their unique binder, as well as module information.
(=^=) :: Var -> Var	-> Bool
(=^=) a b	
	=   bind a == bind b
	&&  nameModule a == nameModule b


-- | If the name of this variable includes a namespace qualifier, then set
--	the namespace accordingly and remove the qualifier.
loadSpaceQualifier :: Var -> Var
loadSpaceQualifier var
 = case takeHead (name var) of
	Just '%'	-> var { nameSpace = NameRegion,  name = tail (name var) }
	Just '!'	-> var { nameSpace = NameEffect,  name = tail (name var) }
	Just '$'	-> var { nameSpace = NameClosure, name = tail (name var) }
	_		-> var


-- | Pretty print a variable
instance Pretty Var PMode where
 ppr v
  = case nameModule v of
	ModuleNil		-> ppr $ pprVarSpaced v
	ModuleAbsolute ns	-> 	 punc "." ((map ppr ns) ++ [pprVarSpaced v])
 
pprVarSpaced v
  = case nameSpace v of
	-- if there is no namespace set then print ?? as a warning
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

	-- Prepend a + to witness variables, these only show up in the core.
	NameClass	
	 -> let	(x:_)	= name v
	    in	if isUpper x 
	   		then pprVarName v
	   		else "+" % pprVarName v

	_ 		-> pprVarName v	 


-- | Pretty print a variable name, including its unique binder if requested.
pprVarName v
 = ifMode (elem PrettyUnique)
 	(name v % "_" % bind v)
	(ppr $ name v)


-- Variable Info --------------------------------------------------------------
-- | Extra information about a variable.
data VarInfo
	= ISourcePos	SourcePos	-- ^ Where this var appears in the source.

	| IBoundBy	Var		-- ^ The binding occurance of this var.
	| ISchemeVar	Var		-- ^ If this var was instantiated, the type scheme var it came from.

					--  Only relavent if the var is a type var.
	| IValueVar	Var		-- ^	The value var this type var corresponds to.
	| IValueLiteral	Literal		-- ^	The literal value this type var represents.
	| IParent	Var		-- ^	??

	| IAlias	Var		-- ^ Indicates that this var is an alias/specialisation/instance 
					--	of some other var.

	| ISeaName	String		-- ^ Variable name to use when outputting Sea code.

	| IString	String		-- ^ Some string, for debugging
	deriving (Show)


-- | Pretty print some variable info.
instance Pretty VarInfo PMode where
 ppr x 	= ppr $ show x


-- Module Identifiers ---------------------------------------------------------
data Module
	= ModuleNil			-- ^ No module information. Sometimes this means that the
					--   variable is in the module currently being compiled.
	
	| ModuleAbsolute [String]	-- ^ Absolute module name, of the form "M1.M2.M3 ..."
	deriving (Show, Eq, Ord)


-- | Pretty print a module id.
instance Pretty Module PMode where
 ppr m
  = case m of
	ModuleNil		-> ppr "@ModuleNil"
  	ModuleAbsolute vs	-> "." %!% vs

-- | strip off the module id from a variable
noModule :: Var -> Var
noModule var	= var { nameModule = ModuleNil }

