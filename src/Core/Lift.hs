
module Core.Lift
	(lambdaLiftGlob)
where
import Core.Lift.Base
import Core.Lift.BindTypes
import Core.Lift.LiftLambdas
import DDC.Core.Glob
import DDC.Var
import Util
import qualified Data.Set		as Set
import qualified Data.Map		as Map


-- | Perform lambda lifting on this tree.
--   TODO: This is a mess, and the closure information on the resulting bindings is wrong.
lambdaLiftGlob 	
	:: Glob			-- ^ Header Glob,
	-> Glob			-- ^ Module Glob.
	-> ( Glob		--   transformed module glob, including new lifted bindings.
	   , Set Var)		--   the vars of the new bindings.
	
lambdaLiftGlob
	cgHeader
	cgModule

 = let	(bindsLifted, bindsNew)
		= evalState (lambdaLiftTreeM $ Map.elems $ globBind cgModule) 
		$ initLiftS 
			{ stateHeaderGlob	= cgHeader
			, stateModuleGlob 	= cgModule }
	
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
