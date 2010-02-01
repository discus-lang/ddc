
module Core.Util.Substitute 
	( substituteT
	, substituteVV)
where
	

import Core.Plate.Trans
import Core.Exp

import Util

import Data.Map			(Map)
import Control.Monad.State.Strict
import qualified Data.Map	as Map
import qualified Shared.Var	as Var
import qualified Debug.Trace	as Debug

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
