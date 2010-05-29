
module Core.Util.Substitute 
	( substituteT
	, substituteVV)
where
import Util
import Core.Plate.Trans
import DDC.Var
import DDC.Type
import qualified Data.Map	as Map

substituteT 
	:: (TransM (State ()) a)
	=> Map Var Type
	-> a -> a
	
substituteT sub tt
 = {-# SCC "substituteT" #-} 
   transZ 
 	transTableId 
	 	{ transT	= \x -> return $ subTT sub x }
	tt

subTT sub tt
 	| TVar k (UVar v)	<- tt
	, Just t'		<- Map.lookup v sub	
	= substituteT (Map.delete v sub) t'

	| TVar k (UMore v tMore) <- tt
	, Just t'		 <- Map.lookup v sub	
	= substituteT (Map.delete v sub) t'

	| otherwise					
	= tt


-- substitute variables for variables
substituteVV
	:: (TransM (State()) a)
	=> Map Var Var
	-> a -> a

substituteVV sub xx
	= transZ
 		transTableId
			{ transT	= \x -> return $ subVVinT sub x }
		xx

subVVinT :: Map Var Var -> Type -> Type
subVVinT sub tt
	| TVar k (UVar v1)	<- tt
	, Just v2		<- Map.lookup v1 sub 
	= TVar k $ UVar v2

	| TVar k (UMore v1 tMore) <- tt
	, Just v2		  <- Map.lookup v1 sub
	= TVar k $ UVar v2
	
	| otherwise
	= tt
