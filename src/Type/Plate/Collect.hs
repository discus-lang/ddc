
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


-----
collectBindingVarsT :: Type -> [Var]
collectBindingVarsT tt
	= execState 
		(transZM 
			transTableId 
				{ transT_enter	= collectBindingVarsT' 
				, transF	= collectBindingVarsF' }
			tt)
		[]


collectBindingVarsT' tt
 = case tt of
 	TForall (BVar v) k t		
	 -> do	modify (\s -> s ++ [v])
	 	return tt

 	TForall (BMore v _) k t		
	 -> do	modify (\s -> s ++ [v])
	 	return tt
		
	_ ->	return tt
	
collectBindingVarsF' ff
 = case ff of
 	FWhere (TVar k v) _	
	 -> do	modify (\s -> s ++ [v])
	 	return ff
		
	_ ->	return ff



-----
collectTClassVars :: Type -> [Type]
collectTClassVars tt
 = let collect t
 	= case t of
		TClass{}	
		 -> do	modify (\s -> t : s)
		 	return t
			
		TVar{}	
		 -> do	modify (\s -> t : s)
		 	return t
			
		_ -> 	return t
		
   in	execState
   		(transZM transTableId { transT_enter = collect } tt)
		[]
		
		


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


		

