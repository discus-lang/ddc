
module Type.Util.Generalise
(
--	generaliseT,
)

where

-----
import Util

----
import qualified Shared.Var	as Var
import qualified Shared.VarUtil	as Var
import Shared.Var		(NameSpace(..))

import qualified Data.Set	as Set

import Shared.Error

import Type.Exp
import Type.Pretty
import Type.Plate
import Type.Util.Kind

-----------------------
-- generaliseT
--	Generalise a type.
--	All var kinds are just set to KNil. 
--	Caller will need to fill kind information in later on.
--
--	Don't generalise vars in the environment.
--	Don't generalise regions in the environment of function constructors.
--	Don't generalise regions or colors non-function types.
--
--	BUGS:	genFreeVarsT is too simple. 
--		It won't generalise something like:    
--			forall %r1. List{%r2} ( () -> Int{%r1} )
--
generaliseT	:: [Var]
		-> Type -> Type
		
generaliseT	env t 
 = let	 
 	vsFree		= freeVars t

	-- Don't generalise regions in data types
	envCon		= getConEnv t

	-- 
	funEnv		= getTopClo t
--	funEnvVs	= map (\(NVar v) -> v) funEnv
	funEnvVs	= []

	env'		= funEnvVs ++ envCon ++ env

	vs'		= filter (\v -> not $ elem v env') 
			$ Set.toList vsFree
   in
   	case t of
	 TForall vs t	-> 
	 	case (nub $ map fst vs ++ vs') of
		 []	-> t
		 vars 	-> TForall varKinds t
		 	where
				vars'		= Var.sortForallVars $ nub $ vars
				varKinds	= zip vars' $ map defaultKindV vars'
				
		 
	 _		->
	 	case vs' of
		 []	-> t
		 vars	-> TForall varKinds t
		 	where
				vars'		= Var.sortForallVars $ nub  $ vars
				varKinds	= zip vars' $ map defaultKindV vars'
		 

genFreeVarsT ::	Type -> [Var]
genFreeVarsT	t 
 = case t of
	-- not a function type, don't generalise its regions.
 	TData{}	-> vsFree'
	 where
		filt v	= (Var.nameSpace v /= Var.NameRegion)
		vsFree'	= filter filt $ Set.toList $ freeVars t
	
	-- A variable.
	TVar k v 	-> Set.toList $ freeVars t
	_		-> Set.toList $ freeVars t


-----
-- getConEnv
--	Can't generalise the regions in a non-function type.
--
getConEnv ::	Type -> [Var]
getConEnv	t
 = case t of
 	TData v ts	-> catMap getConEnv ts
	
	TVar k v
	 |  Var.nameSpace v == NameRegion
	 -> [v]
	 
	 |  otherwise
	 -> []

	TFun{}	-> []
	_	-> []


-----
-- getTopEnv
--	Can't generalise regions that are free in the top level function.
--
getTopClo ::	Type -> Closure
getTopClo	t
 = case t of
 	TFun _ _ _ clo	-> clo
	_		-> TNil


collectClo::	Type -> Maybe Closure
collectClo	t
 = case t of
	TFun _ _ _ clo 	-> Just clo
	_ 		-> Nothing
 
	 
