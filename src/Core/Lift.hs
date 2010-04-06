
module Core.Lift
	(lambdaLiftTree)
where
import Core.Glob
import Core.Lift.Base
import Core.Lift.BindTypes
import Core.Lift.LiftLambdas
import Util
import DDC.Var
import qualified Data.Set		as Set
import qualified Data.Map		as Map


-- | Perform lambda lifting on this tree.
lambdaLiftTree 	
	:: Glob			-- ^ Header Glob,
	-> Glob			-- ^ Module Glob.
	-> ( Glob		--   transformed module glob, including new lifted bindings.
	   , Set Var)		--   the vars of the new bindings.
	
lambdaLiftTree
	cgHeader
	cgModule

 = let	vsBoundTop
		= Set.unions
		[ topBoundVarsOfGlob cgHeader
		, topBoundVarsOfGlob cgModule ]

	(bindsLifted, bindsNew)
		= evalState (lambdaLiftTreeM $ Map.elems $ globBind cgModule) 
			$ initLiftS { stateTopVars = vsBoundTop }
	
	cgBindsLifted	= globOfTree bindsLifted
	cgBindsNew	= globOfTree bindsNew
	
	cgModule' 	= cgModule
			{ globBind	= Map.union 
						(globBind cgBindsLifted)
						(globBind cgBindsNew) 
			}

	vsNewBinds	= Set.fromList 
			$ Map.keys 
			$ globBind cgBindsNew 
		
   in	(cgModule', vsNewBinds)
	

topBoundVarsOfGlob :: Glob -> Set Var
topBoundVarsOfGlob glob
	= Set.unions
		[ Set.fromList $ Map.keys $ globExternData glob
		, Set.fromList $ Map.keys $ globExtern 	   glob
		, Set.fromList $ Map.keys $ globData       glob 
		, Set.fromList $ Map.keys $ globDataCtors  glob
		, Set.fromList $ Map.keys $ globBind	   glob ]
		
lambdaLiftTreeM	
	binds
 = do
	-- Bind all the types in the tree
	--	This adds types for all bound variables to the 
	--	stateTypes member of LiftS
	bindTypesTree binds

	-- lift out lambdas from binds, leaving supers.
	(bindsL, pssNew)
			<- liftM unzip
			$  mapM liftLambdasP binds

	let psNew	= concat pssNew
	
	
	return	(bindsL, psNew)
