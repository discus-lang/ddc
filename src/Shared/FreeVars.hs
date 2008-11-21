
module Shared.FreeVars
	(FreeVars, freeVars)
where

import Data.Set			(Set)
import qualified Data.Set as Set
import Shared.Var

class FreeVars a where
 freeVars :: a -> Set Var
 
instance FreeVars a => FreeVars [a] where
 freeVars xx	= Set.unions $ map freeVars xx
