
module Type.Util.Mask
	( maskReadWriteNotIn )
where
import Type.Util.Bits
import Util
import DDC.Var
import DDC.Type.Exp
import DDC.Type.Builtin
import qualified Data.Set 	as Set


-----
-- | mask Read and Write that aren't on regions in this set.
maskReadWriteNotIn 
	:: Set Var -> Effect -> Effect

maskReadWriteNotIn rsKeep eff
 = let	maskE e
		| TApp t1 (TVar kRegion (UVar r))	<- e
		, elem t1 [tRead, tWrite]
		, not $ Set.member r rsKeep
		= tPure
	
		| otherwise
		= e
	
	esBits	= flattenTSum eff	
	esBits'	= map maskE esBits
	
   in	makeTSum kEffect esBits'

