{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# OPTIONS -O2 #-}

-- | Variables, both bound and binding occurrences.
--   	We also use the `Var` type for constructor and field names, 
--	even though those aren't really variables.
module DDC.Var
	( module DDC.Var.VarId
	, module DDC.Var.NameSpace
	, module DDC.Var.ModuleId
	, Var 		(..)
	, VarInfo	(..)
	, varWithName
	, varWithoutModuleId
	, varsMatchByName
	, loadSpaceQualifier
	, valueParentOfVar
	, takeSeaNameOfVar
	, takeSourcePosOfVar)
where
import DDC.Var.ModuleId
import DDC.Var.NameSpace
import DDC.Var.VarId
import DDC.Base.SourcePos
import DDC.Main.Pretty
import DDC.Main.Error
import Data.Char
import Util

stage	= "DDC.Var"

-- | Variables.
data Var =
	Var 
	{ varName	:: !String		-- ^ Name of this var.
	, varModuleId	:: !ModuleId		-- ^ The module that this var is defined in.
	, varNameSpace	:: !NameSpace		-- ^ The namespace of the var.
	, varId		:: !VarId		-- ^ A unique identifier for the binding occurance.
	, varInfo	:: ![VarInfo] 		-- ^ Some optional info about the var.
	}
	deriving Show


-- | Extra information about a variable.
--	We keep this info in a separate list as opposed to directly in the `Var` type, 
--	because not every var has all the info, and we want to keep the size of the 
--	runtime object down.
data VarInfo
	= ISourcePos	SourcePos		-- ^ Where this var appears in the source program.
	| IBoundBy	Var			-- ^ The binding occurance of this var.
	| IValueVar	Var			-- ^ Type varible, then this gives the value variable.
	| ISeaName	String			-- ^ Variable name to use when outputting Sea code.
        | ISeaGlobal	Bool			-- ^ Is the variable global in the output Sea code.
	deriving (Show)


-- | Create a new variable with this name.
--	The unique binder is set to `VarIdNil`.
varWithName ::	String -> Var
varWithName name	
	= Var 
	{ varName 	= name
	, varModuleId	= ModuleIdNil
	, varNameSpace	= NameNothing	
	, varId		= VarIdNil
	, varInfo	= [] }


-- | Comparing variables for equality.
instance Eq Var where
  {-# INLINE (==) #-}
  (==) v1 v2	
	=   varId v1 == varId v2
	&&  varModuleId v1 == varModuleId v2


-- | Ordering of variables.
instance Ord Var where
 {-# INLINE compare #-}
 compare v1 v2 	
  = case compare (varId v1) (varId v2) of
	EQ	-> compare (varModuleId v1) (varModuleId v2)
	ord'	-> ord'


-- | Pretty print a variable.
instance Pretty Var PMode where
 ppr v
  = case varModuleId v of
	ModuleIdNil	-> ppr $ pprVarSpaced v
	ModuleId ns	-> punc "." ((map ppr ns) ++ [pprVarSpaced v])
 
pprVarSpaced v
  = case varNameSpace v of
	-- if there is no namespace set then print ?? as a warning
	NameNothing	-> "??" % pprVarName v

  	NameValue
	 -> case varName v of
	  	(v1:_)
 	 	 | not $ isAlpha v1	-> parens $ pprVarName v
		 | otherwise		-> pprVarName v

		[] -> panic stage $ "prettyVarN: null var " % show v

	NameType
	 -> case varName v of
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
	 -> let	(x:_)	= varName v
	    in	if isUpper x 
	   		then pprVarName v
	   		else "+" % pprVarName v

	_ 		-> pprVarName v	 


-- | Pretty print a variable name, including its unique binder if requested.
pprVarName v
 = ifMode (elem PrettyUnique)
 	(varName v % "_" % varId v)
	(ppr $ varName v)


-- | Pretty print some variable info.
instance Pretty VarInfo PMode where
 ppr x 	= ppr $ show x


-- | If the name of this variable includes a namespace qualifier, then set
--	the namespace accordingly and remove the qualifier.
loadSpaceQualifier :: Var -> Var
loadSpaceQualifier var
 = case takeHead (varName var) of
	Just '%'	-> var { varNameSpace = NameRegion,  varName = tail (varName var) }
	Just '!'	-> var { varNameSpace = NameEffect,  varName = tail (varName var) }
	Just '$'	-> var { varNameSpace = NameClosure, varName = tail (varName var) }
	_		-> var


-- | Replace the `ModuleId` of a varible with `ModuleIdNil`.
varWithoutModuleId:: Var -> Var
varWithoutModuleId var
	= var { varModuleId = ModuleIdNil }


-- | Check whether the names, namespaces and moduleids of some var match,
--   but don't look at the uniqueid.
varsMatchByName :: Var -> Var -> Bool
varsMatchByName v1 v2 
	=  varName v1      == varName v2
	&& varModuleId v1  == varModuleId v2
	&& varNameSpace v1 == varNameSpace v2


-- | For bound occurrences of value variables, get the binding occurrence.
--   For type variables, get the binding occurence of the value.
--   Good for finding the original source location that gave rise to a variable. 
valueParentOfVar :: Var -> Var
valueParentOfVar var
	| vBinding : _	<- [ v | IBoundBy v <- varInfo var]
	= valueParentOfVar vBinding
	
	| vValue   : _	<- [ v | IValueVar v <- varInfo var]
	= valueParentOfVar vValue
	
	| otherwise
	= var
	

-- | Take the `ISeaName` info from a `Var`.
takeSeaNameOfVar :: Var -> Maybe String
takeSeaNameOfVar var
	| name : _	<- [v | ISeaName v <- varInfo var]
	= Just name

	| name : _	<- [name |  ISeaName name 
			 	<- concat $ [varInfo bound | IBoundBy bound <- varInfo var]]
	= Just name
	
	| otherwise
	= Nothing


-- | Take the `ISourcePos` info from a `Var`.
takeSourcePosOfVar :: Var -> Maybe SourcePos
takeSourcePosOfVar var
	| sp  : _	<- [sp | ISourcePos sp <- varInfo $ valueParentOfVar var]
	= Just sp
	
	| otherwise
	= Nothing


