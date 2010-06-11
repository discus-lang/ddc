
-- | Lambda lifter state.
module Core.Lift.Base
	( LiftS(..)
	, LiftM
	, initLiftS
	, bindType
	, getType
	, getKind
	, newVar
	, addChopped
	, getChopped)
where
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Core.Exp
import DDC.Core.Glob
import DDC.Type
import DDC.Var
import Util
import qualified Shared.Unique	as Unique
import qualified Data.Map	as Map

stage	= "Core.Lift.Base"


-- | Lambda lifter state monad.
type LiftM = State LiftS	
data LiftS
	= LiftS
	{  
	-- | For generating new variable names.
	  stateVarGen		:: VarId

	-- | All types bound in the module.
	, stateTypes		:: Map Var Type

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
	, stateTypes		= Map.empty
	, stateHeaderGlob	= globEmpty
	, stateModuleGlob	= globEmpty
	, stateChopped		= [] 
	}


-- | Add a typed variable to the state
bindType :: Var -> Type -> LiftM ()
bindType v t
 	= modify (\s -> s 
		{ stateTypes 	= Map.insert v t (stateTypes s) })
		

-- | Get the type of some variable from the state.
getType :: Var -> LiftM Type
getType	 v
 = case varNameSpace v of
	NameValue	
	 -> do	t	<- liftM (fromMaybe TNil)
			$  liftM (Map.lookup v)
			$  gets stateTypes
			
		return t
	
	_ -> panic stage $ "getType: no type for " % v % " space = " % show (varNameSpace v)
	

-- | Get the kind of some variable by examining its namespace.
getKind :: Var -> LiftM Kind
getKind	 v
 = case varNameSpace v of
	NameType	-> return kValue
 	NameRegion	-> return kRegion
	NameEffect	-> return kEffect
	NameClosure	-> return kClosure

	-- doh
	NameClass	-> return KNil
	

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


