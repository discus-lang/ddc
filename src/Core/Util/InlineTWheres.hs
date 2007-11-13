
module Core.Util.InlineTWheres
	( inlineTWheresT )
where

import qualified Data.Map	as Map
import Data.Map			(Map)

import Util
import Shared.Error
import Core.Exp

-----
stage	= "Core.Util.InlineTWheres"

-----
-- inlineTWheresT
--	Inline all TLet expressions in this type.
--	
inlineTWheresT :: Map Var Type -> Type 	-> Type
inlineTWheresT sub tt
 = case tt of
 	TNil			-> tt
	
	TForall v k t
	 -> let	t'	= inlineTWheresT sub t
	    in	TForall v k t'
	    
	TWhere t1 vts		
	 -> inlineTWheresT (Map.union (Map.fromList vts) sub) t1

	TContext l t
	 -> let t'	= inlineTWheresT sub t
	    in	TContext l t'

	TSum k ts
	 -> let	ts'	= map (inlineTWheresT sub) ts
	    in	TSum k ts'

	TVar k v	
	 -> case Map.lookup v sub of
	 	Just t	-> t
		_	-> tt
		
    
	-- data
	TFunEC t1 t2 eff clo
	 -> let	t1'	= inlineTWheresT sub t1
	 	t2'	= inlineTWheresT sub t2
		eff'	= inlineTWheresT sub eff
		clo'	= inlineTWheresT sub clo
	    in	TFunEC t1' t2' eff' clo'

	TData v ts
	 -> let	ts'	= map (inlineTWheresT sub) ts
	    in	TData v ts'


	-- region
	
	
	-- effect
	TEffect  v ts
	 -> let	ts'	= map (inlineTWheresT sub) ts
	    in	TEffect v ts'

 	TPure		-> tt
	 	
	-- closure
	TFree v t
	 -> let t'	= inlineTWheresT sub t
	    in	TFree v t'

	TEmpty		-> tt

	TKind k		-> tt
	    
	_ -> panic stage
		$ "inlineTWheresT: no match for " % show tt
