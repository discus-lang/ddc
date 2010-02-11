
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
	, bindN,	bindZ,		bindV
	, lookupN,	lookupZ,	lookupV
	, lbindN,	lbindZ,		lbindV
	, lbindN_shadow, lbindZ_shadow

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
	
	


-- Binding ----------------------------------------------------------------------------------------

-- Top Level Scope --------------------
-- | Bind a variable into the top-level namespace associated with its namespace annotation.
--	It's ok to have multiple variables at top-level with the same name, provided
--	they are in different modules.
--
-- bindTopLevelZ :: Var -> RenameM Var
-- bindTopLevelZ v	= bindN (Var.nameSpace v) v

-- bindTopLevelN :: NameSpace -> Var -> RenameM Var
-- bindTopLevelN space var



-- Local Scope ------------------------
-- | Bind this variable into the local scope associated with its namespace annotation.
--	Causes an error if the variable is already bound at this level.
--
bindZ :: Var -> RenameM Var
bindZ v 	= bindN (Var.nameSpace v) v

-- | Bind a variable into the value namespace.
bindV		= bindN NameValue
	
-- | Bind a variable into a given namespace.
bindN :: NameSpace -> Var -> RenameM Var
bindN space var
 = do	
	-- grab the variable stack for the current namespace.
	Just (spaceMap:ms)
		<- liftM (Map.lookup space)
		$  gets stateStack

	-- ff the variable is already bound at the current level then
	--	we have a "Redefined variable" error.
	(case Map.lookup (Var.name var) spaceMap of
	 	Just boundVar	-> addError $ ErrorRedefinedVar boundVar var
	 	Nothing		-> return ())
		
	-- rename the variable to give it a new unique id.
	varRenamed	<- renameVarN space var

	-- insert the renamed variable into the variable stack.
	let spaceMap'	= Map.insert (Var.name var) varRenamed spaceMap
	modify (\s -> s { 
		stateStack	= Map.insert space 
					(spaceMap':ms) 
					(stateStack s)})
	
	-- return the renamed variable.
	return varRenamed


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


-- Variable Lookup --------------------------------------------------------------------------------

-- | Lookup a variable name from the current scope. If it's there then
--	rename this var after it, if not then generate an error.
lookupZ :: Var -> RenameM Var
lookupZ	v	= lookupN (Var.nameSpace v) v

lookupV		= lookupN NameValue


-- | Link this var to the binding occurance with the same name.
linkBoundVar 
	:: Bool 			-- ^ whether to look in enclosing scopes
	-> NameSpace 			-- ^ namespace to look in
	-> Var 				-- ^ a variable with the name we're looking for
	-> RenameM (Maybe (Var, Var))	-- ^ binding occurance, renamed var (if found)

linkBoundVar enclosing space var
 = do	
 	-- grab the context stack for the appropriate namespace
	Just spaceStack@(spaceMap:_)
		<- liftM (Map.lookup space)
		$  gets stateStack

	-- Check for redefined Data and Constructor names.
	(case (isCtorName var, space, Map.lookup (Var.name var) spaceMap) of
		(True, NameType, Just boundData)
			->	addError $ ErrorRedefinedData boundData var

		(True, NameValue, Just boundCtor)
			->	addError $ ErrorRedefinedCtor boundCtor var
		(_, _, _)
                	-> return ())

	-- try and find the binding occurance for this variable.
	let var_prim	= fromMaybe var (renamePrimVar space var)				
	let mBindingVar	= lookupBindingVar enclosing (Var.name var_prim) spaceStack

	case mBindingVar of

	 -- found it
	 Just bindingVar	
	  -> return 
	  $  Just 
	    	( bindingVar
		, var	{ Var.name	 = Var.name       bindingVar
			, Var.bind 	 = Var.bind 	  bindingVar
			, Var.nameModule = Var.nameModule bindingVar
			, Var.nameSpace  = Var.nameSpace  bindingVar 
			, Var.info       = Var.info var ++ [Var.IBoundBy bindingVar]})
				
	 -- no binding occurance :(
	 Nothing
	  -> return $ Nothing


-- | Try to find the binding occurance of this variable in the current scope.
lookupBindingVar 
	:: Bool 		-- ^ whether to look in enclosing scopes
	-> String 		-- ^ the name of the variable to look for
	-> [Map String Var] 	-- ^ stack of renamer contexts
	-> Maybe Var		-- ^ the binding occurance 

lookupBindingVar enclosing s []		
	= Nothing

lookupBindingVar enclosing s (m:ms)	
	-- see if it's bound in the current scope.
	| Just v	<- Map.lookup s m 
	= Just v
	
	-- recurse in to enclosing scopes.
	| enclosing
	= lookupBindingVar enclosing s ms
	
	-- not found
	| otherwise
	= Nothing


-- | Try and find the binding occurance of a varwith the same name as this one
--	If it can't be found then add an error to the renamer state and return the original var.

lookupN :: NameSpace ->	Var -> RenameM Var
lookupN	 space var
 = do 	mVar	<- linkBoundVar True space var

	case mVar of
	 Nothing	
	  -> do	addError $ ErrorUndefinedVar var { Var.nameSpace = space }
		return var
		
	 Just (_, var')	-> return var'


-- Lazy bind ---------------------------------------------------------------------------------------
--	See if this variable name is already bound in the current scope.
--	If it isn't then bind it with this namespace, otherwise
--	rename it after the one that's already there.

-- | Lazy bind, using the name and namespace of this variable
lbindZ :: Var -> RenameM Var
lbindZ v	= lbindN' True (Var.nameSpace v) v

lbindN space v	= lbindN' True space v


-- | Lazy bind, shadowing any variable with the same name that is defined in an enclosing scope.
lbindZ_shadow :: Var -> RenameM Var
lbindZ_shadow v	= lbindN' False (Var.nameSpace v) v

lbindN_shadow :: NameSpace -> Var -> RenameM Var
lbindN_shadow space var = lbindN' False space var


-- | Lazy bind in this namespace
lbindN' withEnclosing space var
 = do 	mVar	<- linkBoundVar withEnclosing space var
	
	let result

		-- we don't do module local namespacing yet, so we if we see the same var
		--	defined in a different modules we'll treat is as an error.
		| Just (bindingVar, var')	<- mVar
		, Var.nameModule var /= ModuleNil
		, Var.nameModule var /= Var.nameModule var'
		= do	addError $ ErrorRedefinedVar bindingVar var
			return var
		
		-- var was already bound
		| Just (bindingVar, var')	<- mVar
		= return var'
		
		-- var wasn't bound yet
		| otherwise
		= bindN space var
		
	result


-- | Lazy bind in the value namespace
lbindV :: Var -> RenameM Var
lbindV 		= lbindN NameValue


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

