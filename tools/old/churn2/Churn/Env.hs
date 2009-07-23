
module Churn.Env
where

import Churn.Gen.Base
import Churn.Bits
import Churn.Type

import Type.Exp
import Shared.Var

import qualified Data.Map	as Map
import Data.Map			(Map)


-- The type environment
data Env
	= Env {	envType :: Map Var Type }

initEnv
 = Env { envType	= Map.fromList initEnvList }

initEnvList
 = 	[ (varV "+",	tFun tInt (tFun tInt tInt))
	, (varV "not",	tFun tBool tBool) ]


-- Insert a new binding into the environment.
envInsert :: Var -> Type -> Env -> Env
envInsert v t env
 = Env	{ envType = Map.insert v t (envType env) }
	
-- Select one of the vars from the environment that has this type
--	at random. Nothing if there no var of this type in the environment.
--
genEnvVarOfType :: Env -> Type -> GenM (Maybe Var)
genEnvVarOfType env tt
 = do	let vs	= [ v	| (v, t) <- Map.toList (envType env)
			, t == tt]

	case vs of
	 []	-> 
		return Nothing

	 _	-> 
	  do 	v	<- genChoose vs
		return	$ Just v
