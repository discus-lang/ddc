-----
-- Normalises the names in a type.
--
--
module Type.Util.Normalise
	( normaliseT )

where

-----
import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Shared.Var	as Var

import Type.Exp
import Type.Plate


-----
normaliseT :: 	Type -> Type
normaliseT	tt
 = let	vsBinding		= collectBindingVarsT tt
 	(vsBinding', state')	= runState (mapM bindVar vsBinding) initState
	varMap			= stateVarMap state'
	t2			= transformV (rewriteVarV varMap) tt
 in 	t2

-----
type StateN	= State StateS

data StateS	
	= StateS 
	{ stateGen		:: Map Var.NameSpace Int
	, stateVarMap		:: Map Var Var }
		

initState
 =	StateS
 	{ stateGen		= Map.insert Var.NameType   0
				$ Map.insert Var.NameRegion 0
				$ Map.insert Var.NameEffect 0
				$ Map.insert Var.NameClosure 0
				$ Map.empty
				
	, stateVarMap		= Map.empty }
	
spacePrefix
	= [ (Var.NameType,	"t")
	  , (Var.NameRegion,	"r")
	  , (Var.NameEffect,	"e") 
	  , (Var.NameClosure,   "c") ]


bindVar ::	Var ->	StateN Var
bindVar		var	
 = do
	let space		= Var.nameSpace var
	let (Just prefix)	= lookup space spacePrefix

	(Just i)	<- liftM (Map.lookup space)
			$  gets  stateGen
			
	let var'	= var { Var.name = prefix ++ show i }
	
	modify (\s -> s { stateGen	= Map.insert space (i + 1) (stateGen s)
			, stateVarMap	= Map.insert var var' 	   (stateVarMap s) })
			
	return var'


-----
rewriteVarV m v
 = case Map.lookup v m of
	Nothing	-> v
	Just v'	-> v'


