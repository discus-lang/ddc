
module Type.Plate.FreeVars
(
	freeVarsT,
	freeVarsF
)

where

-----
import Util.List
import Util.Tuple
import Util.Maybe
import Type.Exp

import Shared.Error

-----
stage	= "Type.Plate.FreeVars"

-----
freeVarsT :: Type -> [Var]
freeVarsT t
 = let ?bound = [] 
   in  nub $ free t
 

class Free a where
 free :: (?bound :: [Var]) -> a -> [Var]
 
-----
instance Free Type where
 free t
  = case t of
	TNil
	 -> []

	TForall vs t
	 -> let ?bound 	= map fst vs ++ ?bound
	    in free t

	TFetters fs t
	 -> let ?bound	= [ v | FLet (TVar k v) _ <- fs] ++ ?bound
	    in  free t ++ free fs

	TSum k ts		
	 -> catMap free ts

	TUnify k ts
	 -> catMap free ts
	
	TMask k t1 t2
	 -> free t1 ++ free t2

 	TVar k v	-> free v

	TTop k		-> []
	TBot k		-> []

	-- data
	TFun t1 t2 eff clo
	 -> free t1
	 ++ free t2
	 ++ free eff
	 ++ free clo

	TFunF tsEffClo
	 -> let	(ts, eff, clo)	= unzip3 tsEffClo
	    in	 catMap free ts
	      ++ catMap free eff
	      ++ catMap free clo

	TData v ts	-> catMap free ts
	
	-- effect
	TEffect v ts
	 -> free v ++ free ts
	 
	-- closure
	TFree v t	-> free t
	TTag v		-> []

	-- used in solver
	TClass{}	-> []
	TAccept t	-> free t
	TNode x t	-> free t
	TError{}	-> []
	TFetter f	-> free f
	 
	_ -> panic stage $ "freeT: no match for " ++ show t ++ "\n"	 
	    
-----
instance Free Var where
 free v
	= if elem v ?bound then [] else [v]

instance Free a => Free [a] where
 free ts	= catMap free ts

	
-----
freeVarsF f
 = let ?bound = []
   in free f

instance Free Fetter where
 free f
  = case f of
	FConstraint v ts	-> free v ++ free ts

	FLet (TVar k v) t2
	 -> let ?bound = v : ?bound
	    in	free t2

	FLet t1 t2		
	 -> free t1 ++ free t2

	FProj pj t1 t2 t3 eff clo	
	 -> free t1 ++ free t2 ++ free t3 ++ free eff ++ free clo

