
module Core.Util.Pack
	( packT 
	, flattenT
	, eraseContextsT )
where
	
-----
import Core.Exp
import Core.Pretty
import Core.Plate.Trans
import Core.Plate.FreeVars
import Core.Util.Effect
import Core.Util.Bits

import Shared.Error
import Util
import Util.Graph.Deps

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

-----
stage	= "Core.Util.Pack"



-- | Pack a type into standard form.
packT :: Type -> Type
packT tt
 = let tt'	= packT1 tt
   in  if tt == tt'
   	then tt'
	else packT1 tt'


-- | Flatten a type so that all where bindings are inlined
flattenT :: Type -> Type
flattenT tt
	= packT $ crushEffsT $ inlineTWheresT Map.empty tt

	
 
-- | Do one round of packing
packT1 :: Type -> Type
packT1 tt 
 = case tt of
 	TForall vks t1 t2
	 -> let	t2'	= packT1 t2
	    in	TForall vks t1 t2'
	 
	TContext t1 t2
	 -> let	t1'	= packT1 t1
	 	t2'	= packT1 t2
	    in 	TContext t1' t2'

	TWhere (TVar k v1) [(v2, t2)]
	 | v1 == v2
	 -> t2
	 
	TWhere t1 vts
	 -> let t1'	= packT1 t1
	 	vts'	= restrictBinds t1' vts
	    in	makeTWhere t1' vts'

	TApp t1 t2
	 -> let	t1'	= packT1 t1
	 	t2'	= packT1 t2
	    in	TApp t1' t2'
	    
	TSum k ts
	 -> makeTSum k $ map packT1 ts
	 
	TMask k t1 t2
	 -> let t1'	= packT1 t1
	 	t2'	= packT1 t2
	    in	TMask k t1' t2'
	    
	TVar k v	-> tt
	TBot k		-> tt
	TTop k 		-> tt
	 
	-- data
	TData v ts
	 -> let	ts'	= map packT1 ts
	    in	TData v ts'
	    
	TFunEC t1 t2 eff clo
	 -> let t1'	= packT1 t1
	  	t2'	= packT1 t2
		eff'	= packT1 eff
		clo'	= packT1 clo
	    in	TFunEC t1' t2' eff' clo'
	    
	TFun t1 t2
	 -> let	t1'	= packT1 t1
	 	t2'	= packT1 t2
	    in	TFun t1' t2'
	    
	-- effect
	TEffect v ts
	 -> let	ts'	= map packT1 ts
	    in	TEffect v ts'
	    
	-- closure
	TFree v (TBot KClosure)
	 -> TBot KClosure

	TFree v t1
	 -> let t1'	= packT1 t1
	    in	TFree v t1'
	    
	TTag v	-> tt
	
	-- class
	TClass v ts
	 -> let	ts'	= map packT1 ts
	    in	TClass v ts'
	
	-- kind embedding
	TKind{}	-> tt
	
	-- wildcards
	TWild{}	-> tt

	_ -> panic stage
		$ "packT: no match for " % tt


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

	TMask k t1 t2
	 -> let	t1'	= inlineTWheresT sub t1
	   	t2'	= inlineTWheresT sub t2
	    in	TMask k t1 t2
	    
	TVar k v	
	 -> case Map.lookup v sub of
	 	Just t	-> t
		_	-> tt
		
    	TTop k	-> tt
	TBot k	-> tt
    
    
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
 	
	-- closure
	TFree v t
	 -> let t'	= inlineTWheresT sub t
	    in	TFree v t'

	TTag v		-> tt

	TKind k		-> tt
	    
	_ -> panic stage
		$ "inlineTWheresT: no match for " % show tt



-- | Restrict the list of TLet fetters to ones which are 
--	reachable from this type. Also erase x = Bot fetters.
--
restrictBinds :: Type -> [(Var, Type)] -> [(Var, Type)]
restrictBinds tt ls
 = let	reachFLetsMap
 		= Map.fromList
		$ [(t, freeVarsT tLet)	
 			| (t, tLet)	<- ls]
 
 	vsSeed		= freeVarsT tt

	vsReachable	= vsSeed `Set.union` graphReachableS reachFLetsMap vsSeed
	 
   in	filter	(\(v, _) -> Set.member v vsReachable)
   		ls


eraseContextsT :: Type -> Type
eraseContextsT tt
 = transformT eraseContextsT' tt

eraseContextsT' tt
 = case tt of
 	TContext c t	-> t
	_		-> tt
