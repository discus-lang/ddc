
module DDC.Type.Collect
	( module DDC.Type.Collect.FreeTVars
	, module DDC.Type.Collect.FreeVars
	, module DDC.Type.Collect.Visible
	, module DDC.Type.Collect.Textual
	, module DDC.Type.Collect.Static
	
	, collectBindingVarsT
	, collectTErrors)
where
import DDC.Type.Collect.FreeTVars
import DDC.Type.Collect.FreeVars	()
import DDC.Type.Collect.Visible
import DDC.Type.Collect.Textual
import DDC.Type.Collect.Static
import DDC.Type.Compounds
import DDC.Type.Transform
import DDC.Type.Exp
import DDC.Var
import Control.Monad.State.Strict
import Data.Maybe
import Data.Set			(Set)
import qualified Data.Set	as Set
import qualified Data.Map	as Map

-- | Collect all the binding variables in foralls and where constraints.
collectBindingVarsT :: Type -> Set Var
collectBindingVarsT tt
 = let	collectT tt
  	 = case tt of
		TForall (BVar v) k t		
 	 	 -> do	modify (Set.insert v)
 			return tt

		TForall (BMore v _) k t		
	 	 -> do	modify (Set.insert v)
 			return tt
		
		TConstrain t crs
		 -> do	modify (\s -> Set.unions
			 	[ Set.fromList $ mapMaybe takeVarOfType $ Map.keys $ crsEq   crs
				, Set.fromList $ mapMaybe takeVarOfType $ Map.keys $ crsMore crs
				, s ])
			return tt
		
		_ ->	return tt

   in	execState 
		(transformTM collectT tt)
		Set.empty


-- | Collect all the TErrors in this thing.
--   We can't put them in a Set because Ord is not defined
--   over all constructors of Type.
collectTErrors 
	:: TransM (State [Type]) a
	=> a
	-> [Type]
	
collectTErrors x
 = let	collect t
	 = case t of
		TError{}
		 -> do	modify (\s -> t : s)
			return t
		
		_	-> return t
		
   in	execState
		(transZM (transTableId { transT_enter = collect })
			 x)
		[]
