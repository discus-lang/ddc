
module Source.RenameM
	( RenameS(..)
	, RenameM, runRename
	, initRenameS
	, traceM
	, addError

	, pushObjectVar
	, popObjectVar
	, peekObjectVar

	, Rename (..)

	, isBound_local
	, addN
	, bindN,	bindZ,		bindV
	, lookupN,	lookupZ,	lookupV
	, lbindN,	lbindZ,		lbindV
	, lbindN_shadow, lbindZ_shadow

	, pushN
	, popN
	, local)

where

-----
import Util
import qualified Util.Map	as Map
import Util.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

-----
import qualified Shared.Var	as Var
import Shared.Var		(Var, VarBind, NameSpace(..), (=~=), Module(..))
import Shared.VarPrim		(renamePrimVar)
import Shared.Pretty

import Shared.Error
import Source.Error

import qualified Debug.Trace	as Debug

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
	{ stateTrace		:: [String]
	, stateDebug		:: Bool

	  -- Source level renamer errors found so far.
	, stateErrors		:: [Error]

	  -- Fresh variable generators
	, stateGen		:: Map NameSpace  VarBind			

	  -- A stack of bound variables
	  --	Each level of the stack contains all the vars bound by the same construct, 
	  --	eg, in (\x y z -> e), the vars {x, y, z} are all at the same level.
	  --	It's an error for multiple vars at a binding level to have the same name.
	  --
	, stateStack		:: Map NameSpace  [Map String  Var]	

	  -- A stack holding the current object var.
	  --	Object vars are bound with ~ and referred to with _.
	  --	eg 
	  --		printThing ~thing = print (_field1 % _field2)
	  --
	, stateObjectVar	:: [Var]
	}	

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

	-- Each NameSpace starts with an empty toplevel.
	, stateStack		= Map.fromList
				$ zip	
				[ NameModule
				, NameValue
				, NameType
				, NameRegion
				, NameEffect 
				, NameClosure
				, NameField 
				, NameClass ]
				(repeat [Map.empty])

	, stateObjectVar 	= []
	}

-- | Add an error to the renamer state
addError :: Error -> RenameM ()
addError err
 	= modify $ \s -> s { stateErrors = stateErrors s ++ [err] }

-- | Add a message to the trace in the renamer state.
traceM	::  String -> RenameM ()
traceM ss	= modify (\s -> s { stateTrace = (stateTrace s) ++ [ss] })
		
-- trace ss	= whenM (gets stateDebug) $ traceM $ unlines ss
				
-- | run a renamer computation.
runRename :: RenameM a	-> a
runRename comp		= evalState comp initRenameS 
	
	
-- | Push an object varaible context.
pushObjectVar :: Var	-> RenameM ()
pushObjectVar	v
 	= modify (\s -> s { stateObjectVar = v : stateObjectVar s })
	
-- | Pop an object variable context.
popObjectVar  :: RenameM Var
popObjectVar	
 = do 	objectVar	<- gets stateObjectVar
	case objectVar of
	 []	-> panic stage "popObjectVar: stack underflow\n"
	 (x:xs)	
	  -> do	modify (\s -> s { stateObjectVar = xs })
 	  	return x

-- | Peek at the current object variable context.
peekObjectVar :: RenameM Var
peekObjectVar
 = do 	objectVar	<- gets stateObjectVar
 	case objectVar of
	 []	-> panic stage "peekObjectVar: stack underflow\n"
	 (x:xs)
	  -> 	return x


-- | Bind name
--	Take the namespace annotation on a var and bind it into that space.
--	Creates an error if it's already bound at this level.
bindZ :: Var -> RenameM Var
bindZ	 v 	= bindN (Var.nameSpace v) v

bindV		= bindN NameValue
	
bindN :: NameSpace -> Var -> RenameM Var
bindN	 space var
 = do	-- Grab the current var stack.
	Just (spaceMap:ms)
		<- liftM (Map.lookup space)
		$  gets stateStack

	-- See if this name is already bound at this level.
	(case Map.lookup (Var.name var) spaceMap of
	 	Just boundVar	
		 -> do	modify (\s -> s { 
		 		stateErrors = stateErrors s ++ [ErrorRedefinedVar boundVar var]})

	 	Nothing
		 -> return ())
		
	-- Var isn't bound yet, proceed.
	var'		<- renameVarN space var

	let spaceMap'	= Map.insert (Var.name var) var' spaceMap
	
	modify (\s -> s 
		{ stateStack	= Map.insert space 
					(spaceMap':ms) 
					(stateStack s)})
	return var'


-- | Checks whether this name is already bound at this level
isBound_local :: Var -> RenameM Bool
isBound_local var
 = do	Just (spaceMap:ms)
		<- liftM (Map.lookup $ Var.nameSpace var)
		$  gets stateStack

	case Map.lookup (Var.name var) spaceMap of
 		Just boundVar	-> return True
		_		-> return False


-- | If this is the name of a prim var then give it the appropriate VarId
--	otherwise give it a fresh one. Also set the namespace.
renameVarN ::	NameSpace -> Var -> RenameM Var
renameVarN	space var

	-- If we're trying to rename the var into a different namespace
	--	to the one it already has then something has gone wrong.
	| Var.nameSpace var /= NameNothing
	, Var.nameSpace var /= space
	= panic stage 
	$ "renameVarN: not renaming var " % var % " from space " % Var.nameSpace var
	% " to space " % space % "\n"
	% " var = " % show var % "\n"

	| otherwise
	= case renamePrimVar space var of
	 	Just var'	-> return var' { Var.nameSpace = space }
		Nothing 	-> renameVarN' space var	

renameVarN' space var
 = do	-- grab the VarId generator for this space
 	(Just spaceGen)	<- liftM (Map.lookup space)
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


-- | Add some (already renamed) variables to the current scope.
--	
addN ::	NameSpace -> [Var] -> RenameM ()
addN	space vs
 = do	-- Sanity check.
	--	Vars added directly to the namespace should have already been renamed.
	let vs'	= map (\v -> case Var.nameSpace v of
				NameNothing	-> panic stage $ "addN: var " % v % " is in NameNothing.\n"
				_		-> v)
		$ vs

 	(Just (spaceMap:ms))
		<- liftM (Map.lookup space)
		$  gets stateStack
		
	let spaceMap'	= Map.union spaceMap 
			$ Map.fromList 
			$ zip (map Var.name vs') vs'
	
	modify $ \s -> s 
		{ stateStack	= Map.insert space
					(spaceMap':ms)
					(stateStack s) }
					
	return ()


-- | Lookup a variable name from the current scope. If it's there then
--	rename this var after it, if not then generate an error.
lookupZ :: Var -> RenameM Var
lookupZ	v	= lookupN (Var.nameSpace v) v

lookupV		= lookupN NameValue


-- | Link this var to the binding occurance with the same name.
linkBoundVar 
	:: Bool 			-- whether to look in enclosing scopes
	-> NameSpace 			-- namespace to look in
	-> Var 				-- a variable with the name we're looking for
	-> RenameM 
		(Maybe (Var, Var))	-- binding occurance, renamed var.

linkBoundVar enclosing space var
 = do	
 	-- grab the context stack for the appropriate namespace
	stack		<- gets stateStack
	let spaceStack	= case Map.lookup space stack of
				Just s	-> s
				Nothing	-> panic stage 
					$ "linkBoundVar: no space stack for " % space
					% "    var: " % var 		% "\n"
					% "   info: " % Var.info var	% "\n"

	-- try and find the binding occurance
	let var_prim	= fromMaybe var (renamePrimVar space var)				
	let mBindingVar	= lookupBindingVar enclosing (Var.name var_prim) spaceStack
	case mBindingVar of

	 -- found it
	 Just bindingVar	
	  -> return 
	  $  Just 
	    	( bindingVar
		, var
			{ Var.name	 = Var.name       bindingVar
			, Var.bind 	 = Var.bind 	  bindingVar
			, Var.nameModule = Var.nameModule bindingVar
			, Var.nameSpace  = Var.nameSpace  bindingVar 
			, Var.info       = Var.info var ++ [Var.IBoundBy bindingVar]})
				
	 -- no binding occurance :(
	 Nothing
	  -> return $ Nothing


-- | lookup the binding occurance of a variable with this name in the current
--	scope or any enclosing scopes.
lookupBindingVar 
	:: Bool 		-- whether to look in enclosing scopes
	-> String 		-- the name of the variable to look for
	-> [Map String Var] 	-- stack of renamer contexts
	-> Maybe Var		-- the binding occurance of a variable wih the given name

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


	
-- Utils -------------------------------------------------------------------------------------------

-- | Try and find the binding occurance of a varwith the same name as this one
--	If it can't be found then add an error to the renamer state and return the original var.

lookupN :: NameSpace ->	Var -> RenameM Var
lookupN	 space var
 = do 	mVar	<- linkBoundVar True space var

	case mVar of
	 Nothing	
	  -> do modify $ \s -> s { 
	  		stateErrors 	= (stateErrors s) 
					++ [ErrorUndefinedVar var { Var.nameSpace = space }] }
		return var
		
	 Just (_, var')
	  -> 	return var'


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

		-- we don't do real namespacing yet, so we if we see the same var
		--	defined in different modules we'll treat is as an error.
		| Just (bindingVar, var')	<- mVar
		, Var.nameModule var /= ModuleNil
		, Var.nameModule var /= Var.nameModule var'
		= do	addError
				$ ErrorRedefinedVar
					{ eFirstDefined = bindingVar
					, eRedefined	= var }
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
					$ "popN: namespace " % space % " is empty, cannot pop."
				(m:ms)	-> ms)
			space
			$ stateStack s }


-- | Do some renaming in a local scope
local :: RenameM a -> RenameM a
local f
 = do	let spaces	
 		= [NameValue, NameType, NameRegion, NameEffect, NameClosure, NameField]
 
 	mapM_ pushN spaces
 	x	<- f
	mapM_ popN  spaces
	return x

