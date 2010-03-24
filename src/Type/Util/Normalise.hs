-----
-- Normalises the names in a type.
--
--
module Type.Util.Normalise
	( normaliseT )
where
import Type.Exp
import Type.Plate.Collect
import Type.Plate.Trans
import DDC.Var
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import Util

-----
normaliseT :: 	Type -> Type
normaliseT	tt
 = let	vsBinding	= Set.toList $ collectBindingVarsT tt
 	(_, state')	= runState (mapM bindVar vsBinding) initState
	varMap		= stateVarMap state'
	t2		= transformV (rewriteVarV varMap) tt
 in 	t2

-----
type StateN	= State StateS

data StateS	
	= StateS 
	{ stateGen		:: Map NameSpace Int
	, stateVarMap		:: Map Var Var }
		

initState
 =	StateS
 	{ stateGen		= Map.insert NameType   0
				$ Map.insert NameRegion 0
				$ Map.insert NameEffect 0
				$ Map.insert NameClosure 0
				$ Map.empty
				
	, stateVarMap		= Map.empty }
	
spacePrefix
	= [ (NameType,	"t")
	  , (NameRegion,	"r")
	  , (NameEffect,	"e") 
	  , (NameClosure,   "c") ]


bindVar ::	Var ->	StateN Var
bindVar		var	
 = do
	let space		= varNameSpace var
	let (Just prefix)	= lookup space spacePrefix

	(Just i)	<- liftM (Map.lookup space)
			$  gets  stateGen
			
	let var'	= var { varName = prefix ++ show i }
	
	modify (\s -> s { stateGen	= Map.insert space (i + 1) (stateGen s)
			, stateVarMap	= Map.insert var var' 	   (stateVarMap s) })
			
	return var'


-----
rewriteVarV m v
 = case Map.lookup v m of
	Nothing	-> v
	Just v'	-> v'


