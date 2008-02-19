{-# OPTIONS -fwarn-incomplete-patterns #-}

module Type.Plate.FreeVars
	(FreeVars(..))

where

-----
import Shared.Error
import Type.Exp

import qualified Data.Set	as Set
import Data.Set			(Set, (\\), empty, union, unions, fromList, singleton)

-----
stage	= "Type.Plate.FreeVars"

class FreeVars a where
 freeVars :: a -> Set Var

-----
instance FreeVars a => FreeVars [a] where
 freeVars xx	= unions $ map freeVars xx

-----
instance FreeVars Var where
 freeVars v	= singleton v
 
-----
instance FreeVars Type where
 freeVars tt
  = case tt of
	TNil
	 -> empty

	TForall vks t
	 -> (union (freeVars t) (freeVars $ map snd vks))
	 	\\ (fromList $ map fst vks)

	TFetters fs t
	 -> union (freeVars fs) (freeVars t)
	 	\\ (fromList [ v | FLet (TVar k v) _ <- fs])
		
	TSum k ts		
	 -> freeVars ts
	 
	TMask k t1 t2
	 -> union (freeVars t1) (freeVars t2)

 	TVar k v	
	 -> singleton v

	TTop k	-> empty
	TBot k	-> empty

	-- data
	TFun t1 t2 eff clo
	 -> unions
	 	[ freeVars t1
		, freeVars t2
		, freeVars eff
		, freeVars clo ]

{-	TFunF tsEffClo
	 -> let	(ts, eff, clo)	= unzip3 tsEffClo
	    in unions
	    	[ freeVars ts
		, freeVars eff
		, freeVars clo ]
-}
	TData v ts	
	 -> union (singleton v) (freeVars ts)
	
	-- effect
	TEffect v ts
	 -> union (singleton v) (freeVars ts)
	 
	-- closure
	TFree v t	-> freeVars t

	TDanger t1 t2	
	 -> unions 
	 	[ freeVars t1
		, freeVars t2]

	TTag v		-> empty

	-- wildcards
	TWild{}		-> empty

	-- used in solver
	TClass{}	-> empty
	TAccept t	-> freeVars t
	TNode x t	-> freeVars t
	TError{}	-> empty
	TFetter f	-> freeVars f
	 
	_ -> panic stage $ "freeT: no match for " ++ show tt ++ "\n"	 
	    
-----
instance FreeVars Kind where
 freeVars kk	= empty
	
-----
instance FreeVars Fetter where
 freeVars f
  = case f of
	FConstraint v ts	
	 -> union (singleton v) (freeVars ts)

	FLet (TVar k v) t2
	 -> freeVars t2
	 	\\ singleton v

	FLet t1 t2
	 -> union (freeVars t1) (freeVars t2)
		
	FMore t1 t2
	 -> union (freeVars t1) (freeVars t2)

	FProj pj v tDict tBind
	 -> unions
	 	[ singleton v
		, freeVars tDict
		, freeVars tBind]

	_ -> panic stage $ "free[Fetter]: no match for " ++ show f ++ "\n"


