
module Source.RenameM
	( RenameS(..)
	, RenameM, runRename
	, initRenameS
	, traceM

	, pushObjectVar
	, popObjectVar
	, peekObjectVar

	, Rename (..)

	, addN
	, bindN, lbindN,  pushN, popN, lookupN

	, bindV, lbindV, pushV, popV, lookupV
	, bindT, lbindT, pushT, popT, lookupT
	, bindR, lbindR, pushR, popR, lookupR
	, bindE, lbindE, pushE, popE, lookupE
	, bindC, lbindC, pushC, popC, lookupC

	, lbindF

	, local

	, pushTREC, popTREC
	, pushVTREC, popVTREC
)

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
import Shared.VarPrim		(bindPrimVar)

import Shared.Error
import Source.Error

-----
stage = "Source.RenameM"

-----
class Rename a where
 rename  :: a -> RenameM a

instance Rename a => Rename [a] where
 rename xx	= mapM rename xx

instance Rename a => Rename (Maybe a) where
 rename xx 
  = case xx of
  	Nothing	-> return xx
	Just x	
	 -> do 	x'	<- rename x
		return $ Just x'

-----
-- RenameS
--	The variables for values and types exist in separate name spaces.
--
type	RenameM 	= State RenameS

data	RenameS 
	= RenameS 
	{ stateTrace		:: [String]
	, stateDebug		:: Bool
	, stateErrors		:: [Error]

	  -- Fresh variable generators
	, stateGen		:: Map NameSpace  VarBind			

	  -- The path of the current module.
	, stateCurrentModule	:: Module

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

	, stateCurrentModule	= ModuleNil
			
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

-----
traceM	:: 	String -> RenameM ()
traceM		ss
 	= modify (\s -> s { stateTrace = (stateTrace s) ++ [ss] })
		
trace		ss
	= whenM (gets stateDebug) $ traceM $ unlines ss
				
-----
runRename :: 	RenameM a	-> a
runRename	comp	
	= evalState comp initRenameS 
	
	
-----
pushObjectVar :: Var	-> RenameM ()
pushObjectVar	v
 	= modify (\s -> s { stateObjectVar = v : stateObjectVar s })
	
popObjectVar  :: RenameM Var
popObjectVar	
 = do 	objectVar	<- gets stateObjectVar
	case objectVar of
	 []	-> panic stage "popObjectVar: stack underflow\n"
	 (x:xs)	
	  -> do	modify (\s -> s { stateObjectVar = xs })
 	  	return x
	 
peekObjectVar :: RenameM Var
peekObjectVar
 = do 	objectVar	<- gets stateObjectVar
 	case objectVar of
	 []	-> panic stage "peekObjectVar: stack underflow\n"
	 (x:xs)
	  -> 	return x
	
	
-----
bindN ::	NameSpace -> Var -> RenameM Var
bindN		space 	     var
 = do
	-- Grab the current var stack.
	(Just (spaceMap:ms))
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

-----
renameVarN ::	NameSpace -> 	Var -> RenameM Var
renameVarN	space var
 = case bindPrimVar space var of
 	Just var'	-> return var' { Var.nameSpace = space }
	Nothing 	-> renameVarN' space var

renameVarN'	space var
 = do
	(Just spaceGen)	<- liftM (Map.lookup space)
			$  gets stateGen

	let var'	= var 
			{ Var.bind 		= spaceGen 
			, Var.nameSpace		= space }

	let spaceGen'	= Var.incVarBind spaceGen

	modify (\s -> s
		{ stateGen	= Map.insert space spaceGen' (stateGen s) }) 	
	
	return var'



-----
-- addN
--	| Add some (already renamed) variables to the current scope.
--	
addN ::		NameSpace -> [Var] -> RenameM ()
addN		space vs
 = do

	-- Sanuty check.
	--	Vars added directly to the namespace should have already been renamed.
	--
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
	
	modify (\s -> s {
		stateStack	= Map.insert space
					(spaceMap':ms)
					(stateStack s) })
					
	return ()






-----
lookupNM ::	NameSpace ->	Var -> RenameM (Maybe Var)
lookupNM	space		var
 = do
 	(Just spaceStack)	<- liftM (Map.lookup space)
				$ gets stateStack

	let mBindingVar		= lookupNM' (Var.name var) spaceStack
	
	case mBindingVar of
	 Just bindingVar	
	 	-> return $ 	Just var
				{ Var.bind 	 = Var.bind 	  bindingVar
				, Var.nameModule = Var.nameModule bindingVar
				, Var.nameSpace  = Var.nameSpace  bindingVar 
				, Var.info       = Var.info var ++ [Var.IBoundBy bindingVar]}
				
	 Nothing
	 	-> return $ Nothing


lookupNM' :: String -> [Map String Var] -> Maybe Var
lookupNM' s []		= Nothing
lookupNM' s (m:ms)	
 = case Map.lookup s m of
 	Just v	-> Just v
	Nothing	-> lookupNM' s ms


lookupN ::	NameSpace ->	Var -> RenameM Var
lookupN		space		var
 = do
 	mVar	<- lookupNM space var

	case mVar of
	 Nothing	
	  -> do modify (\s -> s { 
	  		stateErrors 	= (stateErrors s) 
					++ [ErrorUndefinedVar var { Var.nameSpace = space }] })
		return var
		
	 Just var'
	  -> 	return var'


----- 	
lbindN ::	NameSpace ->	Var -> RenameM Var
lbindN		space		var
 = do
 	mVar	<- lookupNM space var
	
	case mVar of
	 Just  	var	-> return var
	 Nothing	-> bindN space var


-----
pushN ::	NameSpace ->	RenameM ()
pushN 		space
 	= modify (\s -> s 
		{ stateStack	= Map.adjust (\ss -> Map.empty : ss) space
				$ stateStack s })
				
-----
popN ::		NameSpace ->	RenameM ()
popN		space
	= modify (\s -> s 
	{ stateStack	
		= Map.adjust 
			(\ss -> case ss of 
				[] 	-> panic stage $ pretty $ "popN: namespace " % space % " is empty, cannot pop."
				(m:ms)	-> ms)
			space
			$ stateStack s })



-----------------------
-- Short forms
--	Using these saves us from some hash in Rename.hs
--

-- value
bindV  		= bindN		NameValue
lbindV 		= lbindN	NameValue
pushV		= pushN		NameValue
popV		= popN		NameValue
lookupV		= lookupN	NameValue

-- type
bindT  		= bindN		NameType
lbindT 		= lbindN	NameType
pushT		= pushN		NameType
popT		= popN		NameType
lookupT		= lookupN	NameType

-- region
bindR  		= bindN 	NameRegion
lbindR 		= lbindN	NameRegion
pushR		= pushN		NameRegion
popR		= popN		NameRegion
lookupR		= lookupN	NameRegion

-- effect
bindE  		= bindN 	NameEffect
lbindE 		= lbindN	NameEffect
pushE		= pushN		NameEffect
popE		= popN		NameEffect
lookupE		= lookupN	NameEffect

-- closure
bindC  		= bindN 	NameClosure
lbindC 		= lbindN	NameClosure
pushC		= pushN		NameClosure
popC		= popN		NameClosure
lookupC		= lookupN	NameClosure

-- field
lbindF		= lbindN	NameField



local f
 = do	pushVTREC
 	x	<- f
	popVTREC
	return x


pushTREC 	= do { pushT; pushR; pushE; pushC; }
popTREC		= do { popT;  popR;  popE;  pushC; }

pushVTREC	= do { pushV; pushT; pushR; pushE; pushC; }
popVTREC	= do { popV;  popT;  popR;  popE;  pushC; }




