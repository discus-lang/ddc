
module Core.Util.Substitute 
	( substituteT
	, substituteVV)
where
import Util
import Core.Plate.Trans
import DDC.Var
import DDC.Type
import DDC.Main.Error
import qualified Data.Map	as Map

stage	= "Core.Util.Substitute"

-- | Substitute types for variables in some thing.
substituteT 
	:: (TransM (State ()) a)
	=> (Var -> Maybe Type)
	-> a 
	-> a
	
substituteT sub tt
 = {-# SCC "substituteT" #-} 
   transZ 
 	transTableId 
	 	{ transT	= \x -> return $ subTT sub x }
	tt

subTT sub tt
 	| TVar k (UVar v)	<- tt
	, Just t'		<- sub v
	= substituteT (block v sub) t'

	| TVar k (UMore v tMore) <- tt
	, Just t'		 <- sub	v
	= substituteT (block v sub) t'

	| otherwise					
	= tt


block :: Var -> (Var -> Maybe Type) -> Var -> Maybe Type
block v1 f v2
{-	| v1 == v2
--	, Just t@(TVar _ (UVar v3)) <- f v1
--	, v3 == v1
	= Just t
-}
	| v1 == v2
	= panic stage $ "Core.Util.Substitute: got loops though " % v1
	
	| otherwise
	= f v2


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
