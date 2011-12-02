
-- | Lambda lifter state.
module Core.Lift.Base
	( LiftS(..)
	, LiftM
	, initLiftS
	, newVar
	, addChopped
	, getChopped)
where
import DDC.Main.Pretty
import DDC.Core.Exp
import DDC.Core.Glob
import DDC.Var
import Util
import qualified Shared.Unique	as Unique
import qualified Data.Map	as Map


-- | Lambda lifter state monad.
type LiftM = State LiftS	
data LiftS
	= LiftS
	{  
	-- | For generating new variable names.
	  stateVarGen		:: VarId

	-- | The glob of the header.
	, stateHeaderGlob	:: Glob

	-- | The glob of the current module. Bindings that are lifted out are added to this.
	, stateModuleGlob	:: Glob

	-- | A list of bindings chopped out on this pass
	--	old name, new (top level) name, expression
	, stateChopped		:: [(Var, Var, Top)] 		
								
	}	
								
-- | Initial lambda lifted state.
initLiftS
	= LiftS
	{ stateVarGen		= VarId ("v" ++ Unique.coreLift) 0
	, stateHeaderGlob	= globEmpty
	, stateModuleGlob	= globEmpty
	, stateChopped		= [] 
	}

-- | Create a new var in a given namespace
newVar :: NameSpace -> LiftM Var
newVar	space
 = do
 	gen		<- gets stateVarGen
	let gen'	= incVarId gen
	let var		= (varWithName $ pprStrPlain gen) 
				{ varId 		= gen 
				, varNameSpace		= space }
	
	modify $ \s -> s { stateVarGen = gen' }
	return	var
	
	
-- | Add a freshly lifted supercombinator to the state.
addChopped :: Var -> Var -> Top -> LiftM ()
addChopped vOld vNew pSuper
  = modify $ \s -> s 
	{ stateModuleGlob
		= let oldGlob	= stateModuleGlob s
		  in  oldGlob { globBind = Map.insert vNew pSuper $ globBind oldGlob }
	
	, stateChopped 
		= stateChopped s ++ [(vOld, vNew, pSuper)] 
	}


-- | Get all the supercombinators that were lifted out on this pass.
getChopped :: LiftM [(Var, Var, Top)]
getChopped	
 = do 	cs	<- gets stateChopped
	modify (\s -> s { stateChopped = [] })
	
	return cs
