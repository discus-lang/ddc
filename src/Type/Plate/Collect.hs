
-- | Utils for collecting various things from type expressions.
module Type.Plate.Collect
	( collectBindingVarsT
	, collectTErrors )
where 
import Util
import DDC.Type.Exp
import DDC.Type.Transform
import DDC.Var
import qualified Data.Set	as Set


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
		
		_ ->	return tt
		
	collectF ff
 	 = case ff of
		FWhere (TVar k (UVar v)) _	
	 	 -> do	modify (Set.insert v)
 			return ff
		
		_ ->	return ff	

   in	execState 
		(transZM (transTableId 
				{ transT_enter	= collectT 
				, transF	= collectF })
			 tt)
		Set.empty


-- | Collect all the TErrors in this type
--	We can't put them in a Set because Ord is not defined
--	over all constructors of Type.
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

