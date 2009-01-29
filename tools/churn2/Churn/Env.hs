
module Churn.Env
where

import Type.Exp
import Shared.Var

import qualified Data.Map	as Map
import Data.Map			(Map)


-- The type environment
data Env
	= Env {	envType :: Map Var Type }

initEnv
	= Env { envType	= Map.empty }