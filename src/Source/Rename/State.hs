
-- | The renamer renames variables so all binding and bound occurances of a variable
--	in the same scope have the same variable ids. This lets us perform substitution
--	later in the compiler without having to worry about scope and variable capture.
--
--	This module defines the renamer monad and utils for renaming individual variables.
--
module Source.Rename.State
	( RenameS(..)
	, RenameM, runRename
	, initRenameS
	, traceM
	, addError

	, Rename (..)

	, isBound_local
	, addN
	, renameVarN

	, local)
where

import Shared.Var		(Var, VarBind, NameSpace(..), (=~=), Module(..))
import Shared.VarPrim		(renamePrimVar)
import Shared.Pretty
import Shared.Error
import Source.Error
import Util
import Util.Data.Map		(Map)
import qualified Shared.Var	as Var
import qualified Util.Data.Map	as Map

import Data.Set			(Set)
import qualified Data.Set	as Set
import qualified Debug.Trace	as Debug
import Shared.VarUtil		(isCtorName)

-----
stage = "Source.RenameM"


-- Rename Class ------------------------------------------------------------------------------------
-- | things that can be renamed
class Rename a where
 rename  :: a -> RenameM a


-- | simple instances
instance Rename a => Rename [a] where
 rename xx	= mapM rename xx

instance Rename a => Rename (Maybe a) where
 rename xx 
  = case xx of
  	Nothing	-> return xx
	Just x	
	 -> do 	x'	<- rename x
		return $ Just x'

-- Renamer State -----------------------------------------------------------------------------------

--	The variables for values and types exist in separate name spaces.
type	RenameM 	= State RenameS

data	RenameS 
	= RenameS
	{ -- | Accumulation of tracing / debugging info.
	  stateTrace		:: [String]

	  -- | Whether to generate tracing / debugging info.
	, stateDebug		:: Bool

	  -- | Renamer errors found so far.
	, stateErrors		:: [Error]

	  -- | Fresh variable generators, one for each namespace.
	, stateGen		:: Map NameSpace  VarBind			


	  -- | Variables defined at top level.
	  --   It's ok to have multiple variables at top level with the same name, 
	  ---  provided they're in different modules.
	, stateTopLevelVars	:: Map NameSpace (Map String [(Module, Var)])

	  -- | A stack of locally bound variables.
	  --	Each level of the stack contains all the vars bound by the same construct, 
	  --	eg, in (\x y z -> e), the vars {x, y, z} are all at the same level.
	  --	It's an error for multiple vars at the same binding level to have the same name.
	  --
	, stateStack		:: Map NameSpace  [Map String Var]	

	  -- | A stack holding the currently opened object.
	  --	Objects are opened with ^ and their fields are referred to with _.
	  --	eg: printThing ^thing = print (_field1 % _field2)
	  --
	, stateObjectVar	:: [Var]
	}	


-- | Initial renamer state.
initRenameS
	= RenameS
	{ stateTrace		= []
	, stateDebug		= True
	, stateErrors		= []

	, stateGen		= Map.fromList
				[ (NameModule,	Var.XBind "mR"  0)
				, (NameValue,	Var.XBind "vR"  0)
				, (NameType,	Var.XBind "tR"  0)
				, (NameRegion,	Var.XBind "rR"  0)
				, (NameEffect,	Var.XBind "eR"  0) 
				, (NameClosure, Var.XBind "cR"  0)
				, (NameField,	Var.XBind "fR"  0) 
				, (NameClass,	Var.XBind "aR"  0) ] 

	-- Each top-level namespaces starts out empty.
	, stateTopLevelVars	= Map.fromList 
				$ zip allRenamedNameSpaces
				$ repeat Map.empty

	-- No local scopes yet
	, stateStack		= Map.fromList
				$ zip allRenamedNameSpaces 
				$ repeat []

	, stateObjectVar 	= []
	}

allRenamedNameSpaces
 = 	[ NameModule
	, NameValue
	, NameType, NameRegion, NameEffect, NameClosure
	, NameField
	, NameClass ]


-- | Add an error to the renamer state
addError :: Error -> RenameM ()
addError err
 	= modify $ \s -> s { stateErrors = stateErrors s ++ [err] }


-- | Add a message to the trace in the renamer state.
traceM	::  String -> RenameM ()
traceM ss	= modify (\s -> s { stateTrace = (stateTrace s) ++ [ss] })
		
						
-- | run a renamer computation.
runRename :: RenameM a	-> a
runRename comp		= evalState comp initRenameS 
	

-- Renaming Individual Vars -----------------------------------------------------------------------

-- | If this is the name of a primitive var then give it the appropriate VarId
--	otherwise give it a fresh one. Also set the namespace.
renameVarN ::	NameSpace -> Var -> RenameM Var
renameVarN space var

	-- if we're trying to rename the var into a different namespace
	--	to the one it already has then something has gone wrong.
	| Var.nameSpace var /= NameNothing
	, Var.nameSpace var /= space
	= panic stage 
		$ "renameVarN: not renaming var " % var % " from space " % show (Var.nameSpace var)
		% " to space " % show space % "\n"
		% " var = " % show var % "\n"

	| otherwise
	= case renamePrimVar space var of
	 	Just var'	-> return var' { Var.nameSpace = space }
		Nothing 	-> renameVarN' space var	

renameVarN' space var
 = do	-- grab the VarId generator for this space
 	Just spaceGen	<- liftM (Map.lookup space)
			$  gets stateGen

	-- rename the var and set its namespace
	let var'	= var 
			{ Var.bind 		= spaceGen 
			, Var.nameSpace		= space }

	-- increment the generator
	let spaceGen'	= Var.incVarBind spaceGen
	modify $ \s -> s
		{ stateGen	= Map.insert space spaceGen' (stateGen s) }
	
	return var'

-- | Checks whether a variable is already bound at this level (in the local scope)
--	Uses the namespace annotation on the variable.
isBound_local :: Var -> RenameM Bool
isBound_local var
 = do	
	-- grab the var map for the variables namespace.
	Just (spaceMap:ms)
		<- liftM (Map.lookup $ Var.nameSpace var)
		$  gets stateStack

	case Map.lookup (Var.name var) spaceMap of
 		Just boundVar	-> return True
		_		-> return False

-- | Add some already renamed variables to the current scope
--	for this namespace.
addN ::	NameSpace -> [Var] -> RenameM ()
addN	space vs
 = do	
	-- sanity check: variables being added tothe namespace should have already been renamed.
	let checkVar v 
		= case Var.nameSpace v of
			NameNothing	-> panic stage $ "addN: var " % v % " is in NameNothing.\n"
			_		-> v
					
	let vsChecked	= map checkVar vs
				

	-- grab the var map for the current namespace.
 	(Just (spaceMap:ms))
		<- liftM (Map.lookup space)
		$  gets stateStack

	-- add he renamed variables to the current scope.
	let spaceMap'	
		= Map.union spaceMap 
		$ Map.fromList 
		$ [ (Var.name v, v) | v <- vsChecked ]
	
	modify $ \s -> s 
		{ stateStack	= Map.insert space
					(spaceMap':ms)
					(stateStack s) }
					
	return ()


-- Scope management --------------------------------------------------------------------------------

-- | Do some renaming in a local scope
local :: RenameM a -> RenameM a
local f
 = do	mapM_ pushN allRenamedNameSpaces
 	x	<- f
	mapM_ popN  allRenamedNameSpaces
	return x

-- | Push this space of names down to create a local scope.
pushN :: NameSpace -> RenameM ()
pushN space
 	= modify 
 	$ \s -> s 
	{ stateStack	= Map.adjust (\ss -> Map.empty : ss) space
			$ stateStack s }
				
-- | Pop this space of names to return to the enclosing scope
popN ::	NameSpace -> RenameM ()
popN	space
	= modify 
	$ \s -> s 
	{ stateStack	
		= Map.adjust 
			(\ss -> case ss of 
				[] 	-> panic stage  
					$ "popN: namespace " % show space % " is empty, cannot pop."
				(m:ms)	-> ms)
			space
			$ stateStack s }

