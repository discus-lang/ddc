
-- | Perform lambda lifting.
--   This lifts all nested functions out to top-level, adding new parameters to bind free
--   variables if needed. The input is required to be in A-nf, so that nested functions
--   always appear as separate bindings.
--
module Core.Lift
	(lambdaLiftGlob)
where
import Core.Lift.Base
import Core.Lift.LiftLambdas
import DDC.Core.Glob
import DDC.Var
import Util
import qualified Data.Set		as Set
import qualified Data.Map		as Map

-- | Perform lambda lifting on this tree.
lambdaLiftGlob 	
	:: Glob			-- ^ Header Glob,
	-> Glob			-- ^ Module Glob.
	-> ( Glob		--   Transformed module glob, including new top-level bindings.
	   , Set Var)		--   The vars of the new top-level bindings.
	
lambdaLiftGlob
	cgHeader
	cgModule

 = let	-- Perform lambda lifting on the module glob.
	-- We get back a list of new top-level bindings, and the transformed old ones.
	(bindsLifted, bindsNew)
		= evalState (lambdaLiftTreeM $ Map.elems $ globBind cgModule) 
		$ initLiftS 
			{ stateHeaderGlob	= cgHeader
			, stateModuleGlob 	= cgModule }
	
	-- Pack the new and transformed bindings back into the module glob.
	cgBindsLifted	= globOfTree bindsLifted
	cgBindsNew	= globOfTree bindsNew
	
	cgModule' 	= cgModule
			{ globBind	= Map.union 
						(globBind cgBindsLifted)
						(globBind cgBindsNew) 
			}

	-- Make a set of vars of the new top-level bindings.
	vsNewBinds	= Set.fromList 
			$ Map.keys 
			$ globBind cgBindsNew 
		
   in	(cgModule', vsNewBinds)
	

lambdaLiftTreeM	binds
 = do	(bindsL, pssNew)
		<- liftM unzip
		$  mapM liftLambdasP binds

	return	(bindsL, concat pssNew)

