
module Core.Lift
	( lambdaLiftTree )

where

import Util
import qualified Debug.Trace	as Debug

-----
import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Shared.VarPrim	as Var
import qualified Shared.Var	as Var
import Shared.Var		(Var, NameSpace(..))

-----
import Core.Exp
import Core.Util

import Core.Lift.Base
import Core.Lift.BindTypes
import Core.Lift.LiftLambdas
import Core.Plate.Trans

-----
debug		= False
trace ss x	
 = if debug
 	then	Debug.trace ss x
	else	x


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
