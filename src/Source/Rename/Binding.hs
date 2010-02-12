
-- Binding of variables into scopes
-- TODO: these functions are named confusingly
--	 reconsider bind, lookup, lbind, link etc.
--	bind = rename binding occurence
--	link = link bound occurence against binding occurence
--	simple lookup
--
--	linkbind	-
--		-- if already in scope then this is a bound occurrence
--		-- otherwise make it binding.


module Source.Rename.Binding
	( bindTopLevelZ, bindTopLevelN
	, bindZ, 	bindN, 		bindV
	, linkN,	linkZ,		linkV
	, lbindN,	lbindZ,		lbindV
	, lbindN_shadow, lbindZ_shadow )
where

import Source.Rename.State
import Shared.Error
import Source.Error
import Shared.Var		(Var, NameSpace(..), Module(..))
import Shared.VarPrim		(renamePrimVar)
import Shared.VarUtil		(isCtorName)
import qualified Shared.Var	as Var

import Util
import Control.Monad.State
import Data.Set			(Set)
import Data.Map			(Map)
import qualified Data.Set	as Set
import qualified Data.Map	as Map


-- Variable Binding -----------------------------------------------------------

-- Top Level Scope --------------------
-- | Bind a variable into the top-level namespace associated with its namespace annotation.
--	It's ok to have multiple variables at top-level with the same name, provided
--	they are in different modules.
--
bindTopLevelZ :: Var -> RenameM Var
bindTopLevelZ v	= bindN (Var.nameSpace v) v

bindTopLevelN :: NameSpace -> Var -> RenameM Var
bindTopLevelN space var
 = do	
	-- grab the variable map for this namespace
	Just nameMap
		<- liftM (Map.lookup space)
		$  gets stateTopLevelVars

	-- grab any variables with this name already bound
	let modVars	
	 	= fromMaybe []
		$ Map.lookup (Var.name var) nameMap
		
	-- check that there isn't already a variable with this name in the same module
	case modVars of
	 [] -> bindTopLevelN_newName space var nameMap modVars	-- TODO: allow same name in other modules
	 _  
	  -> do addError $ ErrorRedefinedVar var var		-- this is wrong
		return var
		
bindTopLevelN_newName space var nameMap modVars
 = do	
	-- rename the variable to give it a unique id.
	varRenamed	<- renameVarN space var
	
	-- insert the renamed variable into the map
	let modVars'	= (Var.nameModule var, var) : modVars
	let nameMap'	= Map.insert (Var.name var) modVars' nameMap

	modify 	$ \s -> s 
		{ stateTopLevelVars
			 = Map.insert space nameMap' (stateTopLevelVars s) }
			
	return varRenamed
	

-- Local Scope ------------------------
-- | Bind this variable into the current local scope associated with its namespace annotation.
--	Also renames the variable so it has a fresh id.
--
--	If the variable is already bound here then add an error to the renamer
--	state and return the original variable.
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
	Just (spaceMap : spacesEnclosing)
		<- liftM (Map.lookup space)
		$  gets stateStack

	-- if the variable is already bound at the current level then
	--	we have a "Redefined variable" error.

	-- check that there isn't already a variable with this name bound here
	case Map.lookup (Var.name var) spaceMap of
	 Nothing          -> bindN_newName space var spaceMap spacesEnclosing
	 Just varExisting
	  -> do	addError $ ErrorRedefinedVar varExisting var
		return var
		
bindN_newName space var spaceMap spacesEnclosing
 = do		
	-- rename the variable to give it a new unique id.
	varRenamed	<- renameVarN space var

	-- insert the renamed variable into the variable stack.
	let spaceMap'	= Map.insert (Var.name var) varRenamed spaceMap
	modify	$ \s -> s 
		{ stateStack	= Map.insert space 
					(spaceMap' : spacesEnclosing) 
					(stateStack s) }
	
	-- return the renamed variable.
	return varRenamed


-- Variable Lookup --------------------------------------------------------------------------------

-- | Link a bound occurrence of a variable against its binding occurrence.
--	The bound occurrence gets the same varid as the binding occurrence.
--	If the binging occurrence can't be found then return the original
--	variable and add an error to the renamer state.
--
linkZ :: Var -> RenameM Var
linkZ	v	= linkN (Var.nameSpace v) v

-- | Link a bound occurrence of a value variable.
linkV :: Var -> RenameM Var 
linkV		= linkN NameValue

-- | Link a bound occurrence of a variable in a given namespace.
linkN :: NameSpace ->	Var -> RenameM Var
linkN	 space var
 = do 	mVar	<- linkMaybeN True space var

	case mVar of
	 Nothing	
	  -> do	addError $ ErrorUndefinedVar var { Var.nameSpace = space }
		return var
		
	 Just (_, var')	-> return var'


-- | Link this var to the binding occurance with the same name.
linkMaybeN 
	:: Bool 			-- ^ whether to look in enclosing scopes
	-> NameSpace 			-- ^ namespace to look in
	-> Var 				-- ^ a variable with the name we're looking for
	-> RenameM (Maybe (Var, Var))	-- ^ the binding occurance, renamed bound var (if found)

linkMaybeN enclosing space var
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


-- Link bind ---------------------------------------------------------------------------------------
--	See if this variable name is already bound in the current scope.
--	If it's not then this one becomes the binding occurrence.
--	If it is, then link it to the binding occurrence.

-- | Link bind, using the name and namespace of this variable
lbindZ :: Var -> RenameM Var
lbindZ v	= lbindN' True (Var.nameSpace v) v

lbindN space v	= lbindN' True space v


-- | Link bind, shadowing any variable with the same name that is defined in an enclosing scope.
lbindZ_shadow :: Var -> RenameM Var
lbindZ_shadow v	= lbindN' False (Var.nameSpace v) v

lbindN_shadow :: NameSpace -> Var -> RenameM Var
lbindN_shadow space var = lbindN' False space var


-- | Link bind in this namespace
lbindN' withEnclosing space var
 = do 	mVar	<- linkMaybeN withEnclosing space var
	
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
