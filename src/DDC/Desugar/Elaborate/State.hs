
-- | State for the elaborator.
module DDC.Desugar.Elaborate.State
	( ElabM
	, ElabS
	, stateInit
	, newVarN)
where
import Control.Monad.State.Strict
import DDC.Var


type ElabM = State ElabS
data ElabS
	= ElabS 
	{ stateVarGen	:: VarId }

-- | Initial state for the elaboratot.
stateInit :: String -> ElabS
stateInit unique
	= ElabS
	{ stateVarGen	= VarId unique 0 }
	

-- | Create a fresh variable in a given `NameSpace`.
newVarN :: NameSpace -> ElabM Var
newVarN space
 = do	vid@(VarId p i)	<- gets stateVarGen
 
	let name	= charPrefixOfSpace space : p ++ show i
	let var		= (varWithName name) 
			{ varId 	= vid
			, varNameSpace 	= space }
	
	modify $ \s -> s { stateVarGen = VarId p (i + 1) }
	
	return var
