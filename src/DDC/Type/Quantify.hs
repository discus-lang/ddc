{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Type.Quantify
	(quantifyVarsT_constrainForm)
where
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.FreeVars	()
import DDC.Var
import DDC.Util.FreeVars
import Util.Graph.Deps
import qualified Shared.VarUtil	as Var
import qualified Data.Map	as Map
import qualified Data.Set	as Set

-- | Add forall quantifiers for the free variables in a type, 
--   using more-than bounded quantification if there are corresponding
--   constraints in the type.
-- 
--   When adding more-than constraints we need to be careful about the order else
--   variables won't be in scope during type application in the core.
--   For example, with:
--
-- @
--	f :: forall !e2 !e3 !e1
--	  .  ...
--	  :-  !e1 :> !{!e2; !e3 }
-- @
--
-- translated to core..
--
-- @
--	f :: forall !e2 !e3 (!e1 :> !{!e2 ; !e3}). ...
--	                             ^^^^^^^^^^^
-- @
--
--	@!e2@ and @!e3@ need to have been substituted when the argument
--	for @!e1@ is applied.
--
quantifyVarsT_constrainForm :: [(Var, Kind)] -> Type -> Type
quantifyVarsT_constrainForm vks tt@(TConstrain _ crs)
 = let
	-- fn to get the vars we want to quantify from a list of types.
	vsQuants ts	= filter (not . Var.isCtorName)
			$ Set.toList 
			$ freeVars ts

	-- build a map of which vars need to come before others
 	deps		= Map.fromListWith (++) 
			$ concat
			$ [zip (repeat v1) [vsQuants t2]
				| (TVar _ (UVar v1), t2) <- Map.toList $ crsMore crs]

	-- sequence the vars according to the dependency map
	vsSequence	= graphSequence deps Set.empty (map fst vks)
	
	-- look the var kinds back up
	vksSequence	= map (\v -> let Just k = lookup v vks
				     in (v, k))
			$ vsSequence
		
   in 	makeTForall_back vksSequence tt


quantifyVarsT_constrainForm vks tt
	= makeTForall_back vks tt
