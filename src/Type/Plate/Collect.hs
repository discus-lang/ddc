
-- | Utils for collecting various things from type expressions.
module Type.Plate.Collect
	( collectTClassVars
	, collectVarsT
	, collectBindingVarsT
	, collectClassIds
	, collectTClasses)
where 

import Util
import Type.Exp
import Type.Plate.Trans
import Control.Monad.State
import qualified Data.Set	as Set
import Data.Set			(Set)


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


-----
collectVarsT ::	Type -> [Var]
collectVarsT t
	= execState 
		(transZM 
			transTableId 
			{ transV = \v -> do { modify (\s -> v : s); return v; } }
			
			t)
		[]
 
-----
collectClassIds 
	:: (TransM (State [ClassId]) a)
	=> a -> [ClassId]

collectClassIds x
 = let	collect cid 
  	 = do	modify (\s -> cid : s)
	 	return cid
		
   in	execState 
   		(transZM transTableId { transCid = collect } x)
		[]

-----
collectTClasses
	:: (TransM (State [Type]) a)
	=> a -> [Type]
	
collectTClasses x
 = let 	collect t
  	 = case t of
	 	TClass{}	
		 -> do	modify (\s -> t : s)
		 	return t
			
		_ ->	return t
	
   in	execState
   		(transZM transTableId { transT_enter = collect } x)
		[]


		

