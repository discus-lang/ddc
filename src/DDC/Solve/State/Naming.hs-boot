
module DDC.Solve.State.Naming where

import DDC.Solve.State.Squid
import DDC.Type
import DDC.Var


lookupVarToClassId
	:: Var -> SquidM (Maybe ClassId)

getCanonicalNameOfClass 
	:: ClassId -> SquidM Var

