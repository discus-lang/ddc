{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Type.Collect.Visible
	(visibleRsT)
where
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Type.Exp
import DDC.Type.Builtin
import DDC.Type.Pretty	()
import Data.Set			(Set)
import qualified Data.Set	as Set

stage	= "DDC.Type.Collect.Visible"

-- | Collect the list of visible regions from the type sig. 
--   We can't just call freeVarsT, because we don't want to get
--   region vars present in the effect portion of the type.
--
visibleRsT :: Type -> Set Type
visibleRsT tt
 = case tt of
	TForall _ _ t	-> visibleRsT t
	TConstrain t _  -> visibleRsT t
	TSum _ ts	-> Set.unions $ map visibleRsT ts

	TVar k _
	 | k == kRegion	-> Set.singleton tt
	
	TVar{}		-> Set.empty
	TCon{}		-> Set.empty

	TApp (TCon (TyConEffect{})) _
	 -> Set.empty

	-- data
	TApp t1 t2
	 -> Set.unions
		[ visibleRsT t1
		, visibleRsT t2 ]
	 
	TError{}	-> Set.empty	
	_
	 -> panic stage
	 	$ "visibleRsT: no match for " % tt


