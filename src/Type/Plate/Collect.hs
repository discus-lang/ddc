
-- | Utils for collecting various things from type expressions.
module Type.Plate.Collect
	( collectBindingVarsT
	, collectTClassVars
	, collectClassIds
	, collectTClasses
	, collectTErrors )
where 
import Util
import Type.Exp
import Type.Plate.Trans
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
		FWhere (TVar k v) _	
	 	 -> do	modify (Set.insert v)
 			return ff
		
		_ ->	return ff	

   in	execState 
		(transZM (transTableId 
				{ transT_enter	= collectT 
				, transF	= collectF })
			 tt)
		Set.empty


-- | Collect all the TVars and TClasses present in this type, 
--	both binding and bound occurrences.
collectTClassVars :: Type -> Set Type
collectTClassVars tt
 = let	collect t
 	 = case t of
		TClass{}	
		 -> do	modify (Set.insert t)
		 	return t
			
		TVar{}	
		 -> do	modify (Set.insert t)
		 	return t
			
		_ -> 	return t
				
   in	execState 
		(transZM (transTableId 
				{ transT_enter = collect }) 
			 tt)
		Set.empty
 

-- | Collect all the classids in this type.
collectClassIds 
	:: TransM (State (Set ClassId)) a
	=> a 
	-> Set ClassId

collectClassIds x
 = let	collect cid 
  	 = do	modify (Set.insert cid)
	 	return cid
		
   in	execState 
   		(transZM (transTableId { transCid = collect })
			 x)
		Set.empty


-- | Collect all the TClasses in this type.
collectTClasses
	:: TransM (State (Set Type)) a
	=> a 
	-> Set Type
	
collectTClasses x
 = let 	collect t
  	 = case t of
	 	TClass{}	
		 -> do	modify (Set.insert t)
		 	return t
			
		_ ->	return t
	
   in	execState
   		(transZM (transTableId { transT_enter = collect }) 
			 x)
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

