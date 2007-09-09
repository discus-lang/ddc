
module Core.Util.InlineTLet
	( inlineTLetT)
where

import qualified Data.Map	as Map
import Data.Map			(Map)

import Util
import Shared.Error
import Core.Exp

-----
stage	= "Core.Util.InlineTLet"

-----
-- inlineTLetT
--	Inline all TLet expressions in this type.
--	
inlineTLetT ::	Map Var Type -> Type 	-> Type
inlineTLetT 	sub tt
 = case tt of
 	TNil			-> tt
	
	TForall v k t
	 -> let	t'	= inlineTLetT sub t
	    in	TForall v k t'
	    
{-	TLet v t1 t2		
	 -> let	t2'	= inlineTLetT (Map.insert v t1 sub) t2
	    in	t2'
-}
	TContext l t
	 -> let t'	= inlineTLetT sub t
	    in	TContext l t'

	TSum k ts
	 -> let	ts'	= map (inlineTLetT sub) ts
	    in	TSum k ts'

	TVar k v	
	 -> case Map.lookup v sub of
	 	Just t	-> t
		_	-> tt
		
    
	-- data
	TFunEC t1 t2 eff clo
	 -> let	t1'	= inlineTLetT sub t1
	 	t2'	= inlineTLetT sub t2
		eff'	= inlineTLetT sub eff
		clo'	= inlineTLetT sub clo
	    in	TFunEC t1' t2' eff' clo'

	TData v ts
	 -> let	ts'	= map (inlineTLetT sub) ts
	    in	TData v ts'


	-- region
	
	
	-- effect
	TEffect  v ts
	 -> let	ts'	= map (inlineTLetT sub) ts
	    in	TEffect v ts'

 	TPure		-> tt
	 	
	-- closure
	TFree v t
	 -> let t'	= inlineTLetT sub t
	    in	TFree v t'

	TEmpty		-> tt

	TKind k		-> tt
	    
	_ -> panic stage
		$ "inlineTLetT: no match for " % show tt
	    


 	

