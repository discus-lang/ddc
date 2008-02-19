
module Shared.Unique
where

import Shared.Var	(NameSpace(..))

sourceAliasX		= "xSA"

typeConstraint		= "TC"

coreLift		= "CL"

seaThunk		= "xST"		-- Expanding Curry/CallApp thunk operations
seaCtor			= "xSC"		-- Expanding Ctor definitions
seaForce		= "xSF"
seaSlot			= "xSS"		-- Putting vars in GC slots.


-- Used by the type constraint solver
typeSolve	= 
	[ (NameType, 	"tTS")
	, (NameRegion,  "rTS")
	, (NameEffect,	"eTS") 
	, (NameClosure,	"cTS") ]


	
