{-# OPTIONS -O2 #-}

module Shared.Var
	( module DDC.Var.VarId
	, module DDC.Var.NameSpace
	, module DDC.Var.ModuleId
	, Var 		(..)
	, VarInfo	(..)
	, new
	, noModule	
	, loadSpaceQualifier)
where
import DDC.Var.ModuleId
import DDC.Var.NameSpace
import DDC.Var.VarId
import DDC.Main.Pretty
import DDC.Base.SourcePos
import Shared.Error
import Data.Char
import Util

stage	= "Shared.Var"

-- | Variables. 
--	Both bound and binding occurrences.
--	We also use Var for data constructor names, even though they're not really variables.
data Var =
	Var 
	{ name		:: !String		-- ^ Name of this var.
	, nameModuleId	:: !ModuleId		-- ^ The module path that this var was defined in.
	, nameSpace	:: !NameSpace		-- ^ The namespace of the variable.
	, varId		:: !VarId		-- ^ A unique identifier for this binding occurance.
	, info		:: ![VarInfo] }		-- ^ some (optional) info about this var.
	deriving Show


-- | Extra information about a variable.
--	We keep this info in a separate list as opposed to directly in the var type, 
--	because not every var has all the info, and we want to keep the size of the 
--	runtime object down.
data VarInfo
	= ISourcePos	SourcePos	-- ^ Where this var appears in the source program.
	| IBoundBy	Var		-- ^ The binding occurance of this var.
	| IValueVar	Var		-- ^ Type varible, then this gives the value variable.
	| ISeaName	String		-- ^ Variable name to use when outputting Sea code.
	deriving (Show)


-- | Create a new variable with this name.
--	The unique binder is set to XNil.
new ::	String -> Var
new	n	
	= Var 
	{ name 		= n
	, nameModuleId	= ModuleIdNil
	, nameSpace	= NameNothing	
	, varId		= VarIdNil
	, info		= [] }


-- | Comparing variables for equality
instance Eq Var where
  (==) v1 v2	
	=   varId v1 == varId v2
	&&  nameModuleId v1 == nameModuleId v2


-- | Ordering of variables
instance Ord Var where
 compare v1 v2 	
  = case compare (varId v1) (varId v2) of
	EQ	-> compare (nameModuleId v1) (nameModuleId v2)
	ord	-> ord


-- | Pretty print a variable
instance Pretty Var PMode where
 ppr v
  = case nameModuleId v of
	ModuleIdNil		-> ppr $ pprVarSpaced v
	ModuleIdAbsolute ns	-> 	 punc "." ((map ppr ns) ++ [pprVarSpaced v])
 
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
 	(name v % "_" % varId v)
	(ppr $ name v)


-- | Pretty print some variable info.
instance Pretty VarInfo PMode where
 ppr x 	= ppr $ show x


-- | If the name of this variable includes a namespace qualifier, then set
--	the namespace accordingly and remove the qualifier.
loadSpaceQualifier :: Var -> Var
loadSpaceQualifier var
 = case takeHead (name var) of
	Just '%'	-> var { nameSpace = NameRegion,  name = tail (name var) }
	Just '!'	-> var { nameSpace = NameEffect,  name = tail (name var) }
	Just '$'	-> var { nameSpace = NameClosure, name = tail (name var) }
	_		-> var


-- | strip off the module id from a variable
noModule :: Var -> Var
noModule var	= var { nameModuleId = ModuleIdNil }


