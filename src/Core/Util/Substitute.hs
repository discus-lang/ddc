
module Core.Util.Substitute 
	( substituteT
	, substituteVV)
where
import Util
import Type.Exp
import Core.Plate.Trans
import Shared.Var		(Var)
import qualified Data.Map	as Map

-----
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
 	| TVar k v		<- tt
	, Just t'		<- Map.lookup v sub	
	= substituteT (Map.delete v sub) t'

	| TVarMore k v tMore	<- tt
	, Just t'		<- Map.lookup v sub	
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
	| TVar k v1		<- tt
	, Just v2		<- Map.lookup v1 sub 
	= TVar k v2

	| TVarMore k v1 tMore	<- tt
	, Just v2		<- Map.lookup v1 sub
	= TVar k v2
	
	| TFree v1 t		<- tt
	, Just v2		<- Map.lookup v1 sub
	= TFree v2 t

	| otherwise
	= tt
