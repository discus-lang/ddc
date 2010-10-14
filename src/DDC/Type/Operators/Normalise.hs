{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- Normalise the names in a type.
module DDC.Type.Operators.Normalise
	(normaliseT)
where
import DDC.Type.Collect
import DDC.Type.Transform
import DDC.Type.Exp
import DDC.Var
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import Util

-- | Normalise the names in a type.
normaliseT :: Type -> Type
normaliseT tt
 = let	vsBinding	= Set.toList $ collectBindingVarsT tt
 	(_, state')	= runState (mapM bindVar vsBinding) initState
	varMap		= stateVarMap state'
   in	transformV (rewriteVarV varMap) tt


-----
type StateN	= State StateS

data StateS	
	= StateS 
	{ stateGen	:: Map NameSpace Int
	, stateVarMap	:: Map Var Var }
		

initState
 =	StateS
 	{ stateGen	= Map.insert NameType   0
			$ Map.insert NameRegion 0
			$ Map.insert NameEffect 0
			$ Map.insert NameClosure 0
			$ Map.empty
				
	, stateVarMap	= Map.empty }
	
spacePrefix
	= [ (NameType,	  "t")
	  , (NameRegion,  "r")
	  , (NameEffect,  "e") 
	  , (NameClosure, "c") ]


bindVar :: Var -> StateN Var
bindVar	var	
 = do
	let space		= varNameSpace var
	let (Just prefix)	= lookup space spacePrefix

	(Just i)	<- liftM (Map.lookup space)
			$  gets  stateGen
			
	let var'	= var { varName = prefix ++ show i }
	
	modify (\s -> s { stateGen	= Map.insert space (i + 1) (stateGen s)
			, stateVarMap	= Map.insert var var' 	   (stateVarMap s) })
			
	return var'


rewriteVarV m v
 = case Map.lookup v m of
	Nothing	-> v
	Just v'	-> v'


