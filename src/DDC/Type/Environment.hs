{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- | Type environment.
--	Used in Core.Reconstruct and Type.Kind.
--	Not used during type inference, we use the type graph for that.
--
module DDC.Type.Environment
	( Env(..)
	, emptyEnv
	, addEqVT
	, addMoreF
	, addMoreVT
	, addWitnessConst)
where
import DDC.Type.Exp
import DDC.Var
import qualified Data.Map	as Map
import Data.Map			(Map)	
	
-- Env -------------------------------------------------------------------------------------
-- | A table to carry additional information we collect when descending into the tree
--	Eq 		constraints come from type level lets, and value lambda bindings.
--	More (:>) 	come from constraints on type lambdas bindings.
--
data Env
	= Env
	
	-- the name of the function that called this reconstruct.
	-- 	this is printed in panic messages.
	{ envCaller		:: Maybe String

	-- type equalities, T[v1] == t2
	, envEq			:: Map Var Type		

	-- type inequalities, T[v1] :> t2
	, envMore		:: Map Var Type

	-- region Const witnesses
	, envWitnessConst	:: Map Var Var
	
	-- Tells Core.Reconstruct whether to drop an annotation
	--	saying what effects each statement has (for debugging)
	, envDropStmtEff	:: Bool }
	

-- | An empty environment
emptyEnv :: Env
emptyEnv
	= Env
	{ envCaller		= Nothing
	, envEq			= Map.empty
	, envMore		= Map.empty
	, envWitnessConst	= Map.empty
	, envDropStmtEff	= False }


-- | Add a type binding to the environment
addEqVT :: Var -> Type -> Env -> Env
addEqVT v t tt
 = case Map.lookup v (envEq tt) of
	Nothing	-> tt { envEq = Map.insert v t (envEq tt) }
	Just _	-> tt { envEq = Map.insert v t (Map.delete v (envEq tt)) }


-- | Add a type inequality to the environment
addMoreVT :: Var -> Type -> Env -> Env
addMoreVT v t tt
 = case Map.lookup v (envMore tt) of
	Nothing	-> tt { envMore = Map.insert v t (envMore tt) }
	Just _	-> tt { envMore = Map.insert v t (Map.delete v (envMore tt)) }


-- | Add a type inequality from a fetter to the environment
addMoreF :: Fetter -> Env -> Env
addMoreF ff table
 = case ff of
	FMore (TVar _ (UVar v)) t	-> addMoreVT v t table
	_				-> table

-- | Add a Const witness to the environment
addWitnessConst :: Var -> Var -> Env -> Env
addWitnessConst v w tt
 = case Map.lookup v (envWitnessConst tt) of
	Nothing	-> tt { envWitnessConst = Map.insert v w (envWitnessConst tt) }
	Just _	-> tt { envWitnessConst = Map.insert v w (Map.delete v (envWitnessConst tt)) }
