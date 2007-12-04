
module Core.Util.Substitute 
	(substituteT)
where
	
import qualified Data.Map	as Map
import Data.Map			(Map)

import Util
import Core.Plate.Trans
import Core.Exp

import qualified Shared.Var	as Var
import qualified Debug.Trace	as Debug

-----
substituteT 
	:: (TransM (State ()) a)
	=> Map Var Type
	-> a -> a
	
substituteT sub tt
 = transZ 
 	transTableId 
	 	{ transT	= \x -> return $ subTT sub x }
	tt

subTT ::  Map Var Type -> Type -> Type
subTT sub tt
 	| TVar k v		<- tt
	, Just t		<- Map.lookup v sub	= t
	| otherwise					= tt

