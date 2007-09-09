
module Shared.Prim
	( primTypesUnboxed )
where

import	Shared.VarPrim



primTypesUnboxed = 
	[ primTVoidU

	, primTBoolU

	, primTWord8U
	, primTWord16U
	, primTWord32U
	, primTWord64U

	, primTInt8U
	, primTInt16U
	, primTInt32U
	, primTInt64U
	
	, primTFloat32U
	, primTFloat64U
	
	, primTStringU ]
	 
