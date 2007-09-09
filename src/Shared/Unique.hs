
module Shared.Unique
where

import Shared.Var	(NameSpace(..))

-- Variables are of the form
--	<namespace code> <unique code> <other stuff>
--

sourceAliasX		= "xSA"
sourceHacksX		= "xSH"
sourceDesugar		= "SD"

typeConstraint		= "TC"

coreToSea		= "CC"
coreLift		= "CL"
coreSnipFirst		= "CS"		-- First round of snipping.

coreBoxing		= "CB"
coreFullLaziness	= "CF"

seaThunk		= "xST"		-- Expanding Curry/CallApp thunk operations
seaCtor			= "xSC"		-- Expanding Ctor definitions
seaReturn		= "xSR"		-- Make sure we're returning a var.
seaForce		= "xSF"
seaSlot			= "xSS"		-- Putting vars in GC slots.


-- Used by the type constraint solver
typeSolve	= 
	[ (NameType, 	"tTS")
	, (NameRegion,  "rTS")
	, (NameEffect,	"eTS") 
	, (NameClosure,	"cTS") ]


	
