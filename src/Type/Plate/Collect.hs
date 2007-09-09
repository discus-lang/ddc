
module Type.Plate.Collect
	( collectEffsT
	, collectCloT
	, collectTConsT
	, collectTClassVars
	, collectVarsT
	, collectBindingVarsT
	, collectClassIds
	, collectTClasses
	, collectVars
	, collectFetters)
where 

import Util
import Type.Exp
import Shared.Error
import Type.Plate.Trans


collectEffsT :: Type -> [Effect]
collectEffsT t
 = case t of
 	TForall vks t		
	 -> collectEffsT t

	TFun  t1 t2 eff clo
	 -> collectEffsT t1 
	 ++ collectEffsT t2 
	 ++ [eff]

	TData v ts
	 -> concat $ map collectEffsT ts
	 
	_	-> []


-----
collectCloT :: Type -> [Closure]
collectCloT	t
 	= execState (transformTM collectCloT' t) []
	
collectCloT' t
 = case t of
 	TFun _ _ _ clo
	 -> do	modify (\s -> clo : s)
	 	return t
		
	_ -> 	return t


-----
collectTConsT :: Type -> [Type]
collectTConsT    t
	= execState (transformTM collectTConsT' t) []
	
collectTConsT' t
 = case t of
 	TData{}
	 -> do 	modify (\s -> t : s)
	 	return t
	 
	_ -> 	return t


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
 	TForall vks t		
	 -> do	modify (\s -> s ++ map fst vks)
	 	return tt
		
	_ ->	return tt
	
collectBindingVarsF' ff
 = case ff of
 	FLet (TVar k v) _	
	 -> do	modify (\s -> s ++ [v])
	 	return ff
		
	_ ->	return ff



-----
collectTClassVars :: Type -> [Type]
collectTClassVars t
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
   		(transZM transTableId { transT_enter = collect } t)
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
collectVars 
	:: (TransM (State [Var]) a)
	=> a -> [Var]
	
collectVars x
 = let 	collect var
 	 = do	modify (\s -> var : s)
	 	return var

   in	execState
   		(transZM transTableId { transV = collect } x)
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


-----
collectFetters
	:: (TransM (State [Fetter]) a)
	=> a -> [Fetter]
	
collectFetters x
 = let	collect f
 	 = do	modify (\s -> f : s)
	 	return f
		
   in	execState
   		(transZM transTableId { transF = collect } x)
		[]
		
		

