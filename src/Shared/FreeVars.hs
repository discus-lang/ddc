
module Shared.FreeVars
	(FreeVars, freeVars)
where
import DDC.Var
import Util
import qualified Data.Set as Set


-----
class FreeVars a where
 freeVars :: a -> Set Var
 
instance FreeVars a => FreeVars [a] where
 freeVars xx	= Set.unions $ map freeVars xx
