
module Type.Util.Quantify
	(quantifyVarsT)
where

import Type.Plate.FreeVars
import Type.Util.Bits
import Type.Exp
import Util.Graph.Deps
import Util

import qualified Shared.VarUtil	as Var

import qualified Data.Map	as Map
import qualified Data.Set	as Set

-- When quantifying vars we need to arrange that vars free in FMore
--	fetters appear before the bound varaible, else they won't be
--	in scope during type application in the core.
--
-- eg	f :: forall !e2 !e3 !e1
--	  :- ...
--	  ,  !e1 :> !{!e2; !e3 }
--
-- translated to core..
--
--	f = \/ !e2 -> \/ !e3 -> \/ (!e1 :> !{!e2 ; !e3}) -> ...
--                                            ^^^^^^^^^
--	!e2 and !e3 need to have been substituted when the argument
--	for !e1 is applied, else we can't check the effect subsumption.
--

quantifyVarsT :: [(Var, Kind)] -> Type -> Type
quantifyVarsT vks tt@(TFetters t fs)
 = let
	-- build a map of which vars need to come before others
 	deps		= Map.fromListWith (++) 
			$ concat
			$ [zip (repeat v1) [filter (\v -> not $ Var.isCtorName v) 
						$ Set.toList $ freeVars ts]
				| FMore (TVar k v1) ts
				<- fs]

	-- sequence the vars according to the dependency map
	vsSequence	= graphSequence deps Set.empty (map fst vks)
	
	-- look the var kinds back up
	vksSequence	= map (\v -> let Just k = lookup v vks
				     in (v, k))
			$ vsSequence
		
   in {- trace (pprStr 	$ "deps       = " % deps % "\n"
   			% "vsSequence = " % vsSequence	% "\n") $ -}
	-- add the TForall out the front
   	makeTForall vksSequence tt


quantifyVarsT vks tt
	= makeTForall vks tt
