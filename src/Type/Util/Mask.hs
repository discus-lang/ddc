
module Type.Util.Mask
	( maskReadWriteNotIn )

where
import Type.Exp
import Type.Util.Bits
import Shared.VarPrim

import Util
import qualified Data.Set as Set


-----
-- | mask Read and Write that aren't on regions in this set.
maskReadWriteNotIn 
	:: Set Var -> Effect -> Effect

maskReadWriteNotIn rsKeep eff
 = let	maskE e
		| TEffect vE [TVar kRegion r]	<- e
		, elem vE [primRead, primWrite]
		, not $ Set.member r rsKeep
		= TBot kEffect
	
		| otherwise
		= e
	
	esBits	= flattenTSum eff	
	esBits'	= map maskE esBits
	
   in	makeTSum kEffect esBits'

