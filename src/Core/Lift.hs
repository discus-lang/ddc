
module Core.Lift
	( lambdaLiftTree )

where
import Core.Exp
import Core.Util
import Core.Lift.Base
import Core.Lift.BindTypes
import Core.Lift.LiftLambdas
import Core.Plate.Trans

import Shared.Var		(Var, NameSpace(..))
import qualified Shared.VarPrim	as Var
import qualified Shared.Var	as Var

import Util
import Data.Set			(Set)
import Data.Map			(Map)
import Control.Monad.State.Strict
import qualified Data.Set	as Set
import qualified Data.Map	as Map
import qualified Debug.Trace	as Debug

-----------------------
-- lambdaLiftTree
--
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
