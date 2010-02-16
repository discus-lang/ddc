
-- Binding of variables into scopes
module Source.Rename.Binding
	( bindN,  bindZ,  bindV
	, linkN,  linkZ,  linkV
	, lbindN, lbindZ, lbindV)
where

import Source.Rename.State
import Source.Error
import Shared.Error
import Shared.Pretty
import Shared.Var		(Var, NameSpace(..), Module(..))
import Shared.VarUtil		(isCtorName)
import qualified Shared.Var	as Var

import Util
import Control.Monad.State
import Data.Set			(Set)
import Data.Map			(Map)
import qualified Data.Set	as Set
import qualified Data.Map	as Map
import qualified Debug.Trace

-----
stage = "Source.Rename.Binding"
debug		= False
trace s xx	= if debug then Debug.Trace.trace (pprStrPlain s) xx else xx


-- Variable Binding -----------------------------------------------------------

-- These are all just different interfaces onto linkBindN below.
--	Having these makes it easier to write the code in Rename.hs

-- | Bind a varible into the current scope, using the given namespace.
bindN :: NameSpace -> Var -> RenameM Var
bindN space v 	= linkBindN space False v 

-- | Bind a variable into the current scope, using the namespace already on the variable.
bindZ :: Var -> RenameM Var
bindZ v 	= linkBindN (Var.nameSpace v) False v

-- | Bind a variable into the current scope, requiring it to be in the value namespace.
bindV		= linkBindN NameValue False


-- The actual work is done here in linkBindN

-- | Bind this variable into the current scope associated with its namespace annotation.
--	Also renames the variable so it has a fresh id.
--
linkBindN 	
	:: NameSpace 		-- ^ The namespace to bind the variable into.
	-> Bool 		-- ^ Whether to treat as a bound occurrence if already bound at this level.
	-> Var 			-- ^ The variable to bind.
	-> RenameM Var		-- ^ The renamed variable.

linkBindN space linkIfBound var
 = do	
	-- grab the current scope for this namespace.
	scope 	<- getCurrentScopeOfSpace space
	
	-- the top level and local scopes behave differently.
	case scope of
		ScopeTop{}	-> linkBindN_topLevel space linkIfBound var scope
		ScopeLocal{}	-> linkBindN_local    space linkIfBound var scope
		
linkBindN_topLevel space linkIfBound var (ScopeTop mapModVars)
 = do	
	-- grab any variables with this name already bound
	let modVars	
	 	= fromMaybe []
		$ Map.lookup (Var.name var) mapModVars
	
	-- grab the id of the current module from the renamer state
	Just moduleName	<- gets stateModule
	
	-- see if any vars with name are were already bound in the current module
	let varsInSameModule
		= [varX | (moduleX, varX) <- modVars
			, moduleX == moduleName ]

	case varsInSameModule of
	 []		-> linkBindN_topLevel_newName      space var mapModVars modVars
	 [varBinding]	-> linkBindN_topLevel_alreadyBound linkIfBound var varBinding
	 _		-> panic stage 
			$ "linkBindN_topLevel: multiple vars with the same name already bound in this module" 
			% varsInSameModule

linkBindN_topLevel_newName space var mapModVars modVars
 = trace ("linkBindN_topLevel_newName: " % var % " " % Var.info var)
 $ do	
	-- rename the variable to give it a unique id.
	varRenamed	<- uniquifyVarN space var
	
	-- insert the renamed variable back into the current scope
	let modVars'	= (Var.nameModule varRenamed, varRenamed) : modVars
	let mapModVars'	= Map.insert (Var.name varRenamed) modVars' mapModVars

	updateCurrentScopeOfSpace space (ScopeTop mapModVars')
			
	-- return the renamed variable
	return varRenamed

linkBindN_topLevel_alreadyBound False var varBinding
 -- The variable is already bound, and we were told not to link against any binding occurrence
 = do	addError $ ErrorRedefinedVar varBinding var
	return var
	
linkBindN_topLevel_alreadyBound True var varBinding
 = trace ("linkBindN_topLevel_alreadyBound: " % var % " " % Var.info var)
 $ return $ linkBoundAgainstBinding varBinding var

linkBindN_local space linkIfBound var (ScopeLocal mapVar)
 -- See if there is a variable with this name already bound here
 = case Map.lookup (Var.name var) mapVar of
	 Nothing         -> linkBindN_local_newName      space var mapVar
	 Just varBinding -> linkBindN_local_alreadyBound linkIfBound var varBinding

linkBindN_local_newName space var mapVar
 = trace ("linkBindN_local_newName: " % var % " " % Var.info var)
 $ do		
	-- rename the variable to give it a new unique id.
	varRenamed	<- uniquifyVarN space var

	-- insert the renamed variable into the current scope
	let mapVar'	= Map.insert (Var.name var) varRenamed mapVar
	updateCurrentScopeOfSpace space (ScopeLocal mapVar')
	
	-- return the renamed variable.
	return varRenamed
 
linkBindN_local_alreadyBound False var varBinding
 -- The variable is already bound, and we were told not to link against any binding occurrence
 = do	addError $ ErrorRedefinedVar varBinding var
	return var

linkBindN_local_alreadyBound True  var varBinding
 = trace ("linkBindN_local_alreadyBound: " % var % " " % Var.info var)
 $ return $ linkBoundAgainstBinding varBinding var



-- Variable Linking --------------------------------------------------------------------------------

-- | Link a bound occurrence of a variable against its binding occurrence.
--	The bound occurrence gets the same varid as the binding occurrence.
--	If the binging occurrence can't be found then return the original
--	variable and add an error to the renamer state.
--

-- | Link a bound variable against its binding occurrence.
--	Using the namespace already on the variable.
linkZ :: Var -> RenameM Var
linkZ	v	= linkN (Var.nameSpace v) v

-- | Link a bound occurrence of a value variable.
linkV :: Var -> RenameM Var 
linkV		= linkN NameValue

-- | Link a bound occurrence of a variable in a given namespace.
linkN :: NameSpace -> Var -> RenameM Var
linkN space var
 = do	varsBinding <- findBindingVars space (Var.name var)
	case varsBinding of
	 [] -> do
		addError $ ErrorUndefinedVar var { Var.nameSpace = space }
		return var
		
	 [(_, varBinding)]
	  -> return $ linkBoundAgainstBinding varBinding var


linkBoundAgainstBinding :: Var -> Var -> Var
linkBoundAgainstBinding varBinding varBound
 = varBound	
	{ Var.name	 = Var.name       varBinding
	, Var.bind 	 = Var.bind 	  varBinding
	, Var.nameModule = Var.nameModule varBinding
	, Var.nameSpace  = Var.nameSpace  varBinding 
	, Var.info       = Var.info varBound ++ [Var.IBoundBy varBinding]}


-- Finding ----------------------------------------------------------------------------------------
-- | Try to find the binding occurrences of this variable.
--	If multiple modules define a variable with the same name then there will be
--	multiple binding occurrences -- which will be an error.
findBindingVars 
	:: NameSpace			-- ^ the namespace the var was in
	-> String 			-- ^ the name of the variable to look for
	-> RenameM [(Module, Var)]	-- ^ the binding occurrences


findBindingVars space name
 = do	Just scopes	
		<- liftM (Map.lookup space)
		$  gets stateScopes
		
	findBindingVars_withScopes space name scopes

-- Type names are being automagically renamed until we implement real type synonyms.
findBindingVars_withScopes space name scopes
	| NameType	<- space
	= let again name' = findBindingVars_withScopes space name' scopes
          in  case name of 
		"Word"	-> again "Word32"
		"Int"	-> again "Int32"
		"Float"	-> again "Float32"
		"Char"	-> again "Char32"

		"Word#"	-> again "Word32#"
		"Int#"	-> again "Int32#"
		"Float#"-> again "Float32#"
		"Char#"	-> again "Char32#"
	
		_	-> findBindingVars' space name scopes
		
	| otherwise
	= findBindingVars' space name scopes

findBindingVars' space name (ScopeTop mapModVars : [])
 	= return $ fromMaybe [] (Map.lookup name mapModVars)

findBindingVars' space name (ScopeLocal mapVars : scopesEnclosing)
 = case Map.lookup name mapVars of

	-- found it.
	Just varBinding
	 -> do	-- local vars are always in the current module
		Just mod	<- gets stateModule
		return		$ [(mod, varBinding)]
		
	-- there's no variable with this name in the current scope,
	--	so go look in enclosing scopes.
	Nothing
	 -> 	findBindingVars' space name scopesEnclosing


-- Combinations of Linking and Binding ------------------------------------------------------------


-- For the current and enclosing scopes.
-- | Link or bind a variable, using the given namespace.
lbindN space var
 = do	varsBinding <- findBindingVars space (Var.name var)
	case varsBinding of
	 [] 	-> linkBindN space False var
		
	 [(_, varBinding)]
	  	-> return $ linkBoundAgainstBinding varBinding var


-- | Link or bind a variable, using the namespace already on the variable.
lbindZ :: Var -> RenameM Var
lbindZ var	= lbindN (Var.nameSpace var) var

-- | Link or bind a variable, requiring it to be in the value namespace.
lbindV var	= lbindN NameValue var






