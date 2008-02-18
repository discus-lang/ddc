
module Type.Util.Substitute
	(subTT)
where

import Type.Exp

import qualified Data.Map	as Map
import Data.Map			(Map)


subTT 	:: Map Type Type 
	-> Type -> Type
	
subTT sub tt
 = let down	= subTT sub
   in case tt of
 	TForall vks t		-> TForall vks (down t)
	TFetters fs t		-> TFetters fs (down t)
	TSum k ts		-> TSum k (map down ts)
	TMask k t1 t2		-> TMask k (down t1) (down t2)

	TVar{}
	 -> case Map.lookup tt sub of
	 	Nothing		-> tt
		Just tt'	-> subTT (Map.delete tt sub) tt'
		
	TTop{}			-> tt
	TBot{}			-> tt
	
	TData v ts		-> TData v (map down ts)
	TFun t1 t2 eff clo	-> TFun (down t1) (down t2) (down eff) (down clo)
	
	TEffect v ts		-> TEffect v (map down ts)

	TFree v t		-> TFree v (down t)
	TDanger t1 t2		-> TDanger (down t1) (down t2)
	TTag{}			-> tt
	
	TWild{}			-> tt
	
	TClass{}
	 -> case Map.lookup tt sub of
	 	Nothing		-> tt
		Just tt'	-> subTT (Map.delete tt sub) tt'

	TError{}		-> tt
