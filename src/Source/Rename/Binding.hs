
-- Binding of variables into scopes
module Source.Rename.Binding
	( bindN,  bindZ,  bindV
	, linkN,  linkZ,  linkV
	, lbindN_binding, lbindZ_binding, lbindV_binding
	, lbindN_bound,   lbindZ_bound,   lbindV_bound
	, lbindZ_topLevel )
where

import Source.Rename.State
import Source.Error
import Shared.Error
import Shared.Pretty
import Shared.Var		(Var, NameSpace(..), Module(..))
import Shared.VarUtil		(isCtorName)
import qualified Shared.Var	as Var
import qualified Shared.VarPrim	as Var
import qualified Shared.VarUtil	as Var

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

-- | Bind a variable into the current scope, using the namespace already on the variable.
bindZ :: Var -> RenameM Var
bindZ var  	= bindN (Var.nameSpace var) var

-- | Bind a variable into the current scope, requiring it to be in the value namespace.
bindV 		= bindN NameValue

-- | Bind a varible into the current scope, using the given namespace.
bindN :: NameSpace -> Var -> RenameM Var
bindN space var
 = do	
	-- grab the current scope for this namespace.
	scope 	<- getCurrentScopeOfSpace space
	
	case scope of
		ScopeTop{}	-> bindN_topLevel space var scope
		ScopeLocal{}	-> bindN_local    space var scope
		
bindN_topLevel space var (ScopeTop mapModVars)
 = do
	-- grab any variables with this name already bound
	let modVars	
	 	= fromMaybe []
		$ Map.lookup (Var.name var) mapModVars
	
	-- grab the id of the current module from the renamer state
	Just moduleName	<- gets stateModule
	
	-- see if any vars with name are were already bound in the current module
	let varsInSameModule
		= [varX | varX <- modVars
			, Var.nameModule varX == moduleName ]

	case varsInSameModule of
	 [] -> bindN_topLevel_newName space var mapModVars modVars

	 varBinding : _
	  -> do	addError $ ErrorRedefinedVar varBinding var
		return var

bindN_topLevel_newName space var mapModVars modVars
 = trace ("!bindN_topLevel_newName: " % var % " " % Var.info var)
 $ do	
	-- rename the variable to give it a unique id.
	varRenamed	<- uniquifyVarN space var
	
	-- insert the renamed variable back into the current scope
	let modVars'	= varRenamed : modVars
	let mapModVars'	= Map.insert (Var.name varRenamed) modVars' mapModVars

	updateCurrentScopeOfSpace space (ScopeTop mapModVars')
			
	-- return the renamed variable
	return varRenamed

bindN_local space var (ScopeLocal mapVar)
 -- See if there is a variable with this name already bound here
 = case Map.lookup (Var.name var) mapVar of
	 Nothing         -> bindN_local_newName space var mapVar
	 Just varBinding
	  -> do	addError $ ErrorRedefinedVar varBinding var
		return var
		
bindN_local_newName space var mapVar
 = trace ("!linkBindN_local_newName: " % var % " " % Var.info var)
 $ do		
	-- rename the variable to give it a new unique id.
	varRenamed	<- uniquifyVarN space var

	-- insert the renamed variable into the current scope
	let mapVar'	= Map.insert (Var.name var) varRenamed mapVar
	updateCurrentScopeOfSpace space (ScopeLocal mapVar')
	
	-- return the renamed variable.
	return varRenamed
 

-- Finding ----------------------------------------------------------------------------------------
-- | Try to find the binding occurrences of this variable.
--	If multiple modules define a variable with the same name then there will be
--	multiple binding occurrences.
findBindingVars 
	:: NameSpace		-- ^ the namespace the var was in
	-> String 		-- ^ the name of the variable to look for
	-> RenameM [Var]	-- ^ the binding occurrences

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
		return		$ [varBinding]
		
	-- there's no variable with this name in the current scope,
	--	so go look in enclosing scopes.
	Nothing
	 -> 	findBindingVars' space name scopesEnclosing


-- Choosing ---------------------------------------------------------------------------------------
-- | Choose which binding occurrence to use from a number of options.
chooseBindingOccurrence
	:: Module 		-- ^ the current module name
	-> Var 			-- ^ the bound occurrence we're trying to find the binding occurrence for
	-> [Var] 		-- ^ list of binding occurrences to choose from
	-> Either Error (Maybe Var)

chooseBindingOccurrence thisModule var varsBinding

	-- no binding occurrences already in scope. 
	--	This may or may not be an error depending on what we wanted it for.
	| []	<- varsBinding
	= Right Nothing
	
	-- if the bound occurrence has no explicit module name,
	--	and there's a single binding occurrence in the current module then use that.
	| ModuleNil	<- Var.nameModule var
	, [varBinding]	<- filter (\v -> Var.nameModule v == thisModule) varsBinding
	= Right (Just varBinding)
	
	-- if the bound occurrence has no explicit module name,
	--	and there's a single binding occurrence from some other (any) module then use that.	
	| ModuleNil	<- Var.nameModule var
	, [varBinding]	<- varsBinding
	= Right (Just varBinding)
	
	-- if the bound occurrence has an explicit module name,
	--	and there's a single binding occurrence for the same name in that module then use that.
	| ModuleAbsolute _ <- Var.nameModule var
	, [varBinding]	<- filter (\v -> Var.nameModule v == Var.nameModule var) varsBinding
	= Right (Just varBinding)

	-- there are multiple binding occurrences of interest, 
	--	and we have no way of knowing which one to choose.
	| otherwise
	= Left $ ErrorAmbiguousVar varsBinding var


-- Variable Linking --------------------------------------------------------------------------------

-- | Link a bound variable against its binding occurrence.
--	Using the namespace already on the variable.
linkZ :: Var -> RenameM Var
linkZ	v	= linkN (Var.nameSpace v) v

-- | Link a bound occurrence of a value variable.
linkV :: Var -> RenameM Var 
linkV		= linkN NameValue

-- | Link a bound occurrence of a variable against its binding occurrence.
--	The bound occurrence gets the same varid as the binding occurrence.
--	If the binging occurrence can't be found then return the original
--	variable and add an error to the renamer state.
--
linkN :: NameSpace -> Var -> RenameM Var
linkN space var
 = do	varsBinding	<- findBindingVars space (Var.name var)
	Just thisModule	<- gets stateModule
	let var'	= var { Var.nameSpace = space }

	case chooseBindingOccurrence thisModule var' varsBinding of

	 -- no binding occurrences already in scope, its undefined
	 Right Nothing
	  -> do	addError $ ErrorUndefinedVar var'
		return var'
	
	 -- found a suitable binding occurrence
	 Right (Just varBinding)
	  -> return $ linkBoundAgainstBinding varBinding var
	
	 -- found a renamer error of some sort
	 Left err
	  -> do	addError err
		return var		


linkBoundAgainstBinding :: Var -> Var -> Var
linkBoundAgainstBinding varBinding varBound
 = varBound	
	{ Var.name	 = Var.name       varBinding
	, Var.bind 	 = Var.bind 	  varBinding
	, Var.nameModule = Var.nameModule varBinding
	, Var.nameSpace  = Var.nameSpace  varBinding 
	, Var.info       = Var.info varBound ++ [Var.IBoundBy varBinding]}


-- Combinations of Linking and Binding ------------------------------------------------------------

-- These lbind_binding forms are used on binding occurrenes of variables.
-- eg for:
--	fun []     = ...
--      fun (x:xs) = ...
--
-- Both occurrences of 'fun' here are binding occurrences.
-- We call the first one the 'primary' binding occurrence, and all other
--	occurrences (both bound and binding) are linked to it.
--

-- | Link or bind a binding occurrence of a variable,
--	using the namespace already on the variable.
lbindZ_binding :: Var -> RenameM Var
lbindZ_binding var	= lbindN_binding (Var.nameSpace var) var

-- | Link or bind a binding occurrence of a variable, 
--	requiring it to be in the value namespace.
lbindV_binding var	= lbindN_binding NameValue var

-- | Link or bind a binding occurrence of a variabe,
--	using the given namespace
lbindN_binding :: NameSpace -> Var -> RenameM Var
lbindN_binding space var
 -- don't rename already renamed variables
 | Var.bind var /= Var.XNil
 = return var

 | otherwise
 = do	Just thisModule	<- gets stateModule

	varsBinding_thisModule
		<- liftM (filter (\v -> Var.nameModule v == thisModule))
		$ findBindingVars space (Var.name var)

	case varsBinding_thisModule of
	 [] 	-> bindN space var
		
	 [varBinding]
	  	-> return $ linkBoundAgainstBinding varBinding var
		
	 -- as we're only considering binding occurrences in the same module,
	 --	we shouldn't see more than one...
	 vs 	-> panic stage
		$  "lbindN_binding: when linking " % var % " " % Var.info var % "\n"
		%  "    found multiple binding occurrences: " % vs
	
	
---------------------------------------
-- | These lbind_bound forms are used on bound occurrences of variables, 
--	where a binding occurrence may not actually exist.
--
-- eg with:
--	dude :: a -> b
--
--   Variables "a" and "b" should really be bound occurrences, but there is no
--	"forall" providing a binding occurrence.
--

-- | Link or bind a bound occurrence of a variable,
--	using the namespace already on the variable.
lbindZ_bound :: Var -> RenameM Var
lbindZ_bound var	= lbindN_bound (Var.nameSpace var) var

-- | Link or bind a bound occurrence of a variable, 
--	requiring it to be in the value namespace.
lbindV_bound var	= lbindN_bound NameValue var

-- | Link or bind a binding occurrence of a variabe,
--	using the given namespace
lbindN_bound :: NameSpace -> Var -> RenameM Var
lbindN_bound space var
 -- don't rename already renamed variables.
 | Var.bind var /= Var.XNil
 = return var
	
 | otherwise
 = do	varsBinding 	<- findBindingVars space (Var.name var)
	Just thisModule	<- gets stateModule
	let var'	= var { Var.nameSpace = space }
	case chooseBindingOccurrence thisModule var' varsBinding of

	 -- no binding occurrences already in scope,
	 -- 	so make this one the binding occurrence.
	 Right Nothing
	  -> bindN space var
	
	 -- found a suitable binding occurrence
	 Right (Just varBinding)
	  -> return $ linkBoundAgainstBinding varBinding var
	
	 -- found a renamer error of some sort
	 Left err
	  -> do	addError err
		return var
	
	
---------------------------------------
-- | This is called for each top-level variable in the program, before starting renaming proper.
--	We do the start-up process so we can handle mutually defined bindings.
--	This function also checks for errors such as multiply defined types and constructors
--	in the same module.
--
lbindZ_topLevel :: Var -> RenameM Var
lbindZ_topLevel var
 = do	vsBinding 	<- findBindingVars (Var.nameSpace var) (Var.name var)
	Just thisModule	<- gets stateModule

	let (vsBinding_thisModule, _vsBinding_otherModules)
			= partition (\v -> Var.nameModule v == thisModule)
			$ vsBinding
			
	let result

		-- haven't seen this name yet, so add it to the scope
		| []		<- vsBinding_thisModule
		= bindZ var

		-- allow multiple instances of value variables at top level,
		--	we get these due to multiple equations in a function decl.
		| NameValue	<- Var.nameSpace var
		, not $ Var.isCtorName var
		= case vsBinding_thisModule of
		   []			-> bindZ var
		   varBinding : _	-> return $ linkBoundAgainstBinding varBinding var
		
		-- for non-value variables, we can't have multiple bindings
		--	for them at top level.
		| varBinding : _	<- vsBinding_thisModule
		= do	addError $ ErrorRedefinedVar varBinding var
			return var
		
		-- this shouldn't happen
		| otherwise
		= panic stage 
			$ "lbindZ_topLevel: no match with " % var % " " % Var.info var % "\n"
			% "    vsBinding = " % vsBinding % "\n"
				
	result

