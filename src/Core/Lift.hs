
module Core.Lift
	(lambdaLiftTree)
where
import Core.Exp
import Core.Lift.Base
import Core.Lift.BindTypes
import Core.Lift.LiftLambdas
import Type.Exp
import Util
import DDC.Var


-- | Perform lambda lifting on this tree.
lambdaLiftTree 	
	:: [Top]		-- ^ some bindings to lambda lift.
	-> Map Var Type		-- ^ type map.
	-> Set Var		-- ^ all the vars in scope at top level.
	-> ( [Top]		--  original bindings as supers.
	   , [Top])		--  new super
	
lambdaLiftTree
	cBinds
	mapType
	vsBoundTop

 = 	evalState (lambdaLiftTreeM cBinds) 
		$ initLiftS
		{ stateTopVars	= vsBoundTop }
		
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
