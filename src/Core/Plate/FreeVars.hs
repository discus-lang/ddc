{-# OPTIONS -fwarn-incomplete-patterns #-}

module Core.Plate.FreeVars
(
	freeVarsX,
	freeVarsS,
	freeVarsT,
	
	varsBoundByG,
	varsBoundByW
)

where

import qualified Data.Set as Set
import Data.Set			(Set, (\\), unions, fromList, empty, singleton)

import Util			hiding ((\\))

import qualified Shared.Var	as Var
import Shared.Var		(Var, NameSpace(..))
import Shared.Error

import Core.Exp

-----
stage	= "Core.Plate.FreeVars"

-----
freeVarsX :: Exp -> Set Var
freeVarsX xx
 = case xx of
	XNothing	-> empty
	XNil		-> empty
	
	XAnnot	n x	
	 -> freeVarsX x

 	XVar	v	
	 -> fromList [v]

	XLAM v k e
	 -> unions 
	 	[ freeVarsX e ]
		\\ fromList [v]

	XAPP x t
	 -> unions
	 	[ freeVarsX x
		, freeVarsT t ]

	XLam v t e eff clo	
	 -> unions
	 	[ freeVarsT t
		, freeVarsX e ]
		\\ fromList [v]

	XApp x1 x2 eff
	 -> unions
	 	[ freeVarsX x1
		, freeVarsX x2 
		, freeVarsT eff ]

	XTau t x
	 -> unions
	 	[ freeVarsT t
		, freeVarsX x ]

	XTet vts x
	 -> unions
	 	[ freeVarsX x
		, Set.unions $ map (freeVarsT . snd) vts]
		
		\\ (Set.fromList $ map fst vts)

	XLocal v vs x
	 -> freeVarsX x \\ fromList [v]

	XDo xs
	 -> let
	 	bound	= unions
			$ map boundByS xs
			
		free	= unions
			 $ map freeVarsS xs

	    in free \\ bound
		
	XMatch aa eff
	 -> unions
	 	[ freeVarsT eff
		, unions $ map freeVarsA aa ]
		
	XConst c t	
	 -> freeVarsT t

	XLifted v vsFree
	 -> fromList vsFree
	
	XPrim m xs eff
	 -> unions
	 	[ freeVarsM m
		, unions $ map freeVarsX xs
		, freeVarsT eff ]

	-- atoms
	XAtom v xs
	 -> unions
	 	[ fromList [v]
		, unions $ map freeVarsX xs ]

	-- intermediate
	XType t
	 -> freeVarsT t

	_ 	-> panic stage
		$ "freeVarsX: no match for " % show xx



freeVarsM :: Prim -> Set Var
freeVarsM m
 = case m of

	MSuspend v	-> singleton v
	MForce 		-> empty

	MBox t1 t2
	 -> unions
	 	[ freeVarsT t1
		, freeVarsT t2 ]

	MUnbox t1 t2 
	 -> unions
	 	[ freeVarsT t1
		, freeVarsT t2 ]

	MTailCall v	-> singleton v
	MCall 	v 	-> singleton v
	MCallApp v i	-> singleton v
	MApply 	v	-> singleton v
	MCurry 	v i	-> singleton v

	MFun v t	
	 -> unions
	 	[ singleton v
		, freeVarsT t ]



freeVarsS ::	Stmt -> Set Var
freeVarsS   	xx
 = case xx of
	SComment{}	-> Set.empty

 	SBind v x
	 -> unions 
	 	[ freeVarsX x ]
		\\ fromList (maybeToList v)


freeVarsA :: 	Alt -> Set Var
freeVarsA    	a	
 = case a of
 	AAlt gs x
	 -> let
	 	bound	= unions 
			$ map varsBoundByG gs
			
		free	= unions 
		 	[ unions $ map freeVarsG gs
			, freeVarsX x ]
	    in	free \\ bound
				

freeVarsG :: 	Guard -> Set Var
freeVarsG 	g
 = case g of
-- 	GCase w 	-> empty
	GExp  w x
	 -> freeVarsX x
	 	\\ varsBoundByW w
		

-----
freeVarsT ::	Type -> Set Var
freeVarsT	tt
 = case tt of
	TNil		-> empty
	TNothing	-> empty


 	TForall v k t	
	 -> freeVarsT t
	 	\\ (Set.fromList [v])

	TContext l t
	 -> unions
	 	[ freeVarsT l
		, freeVarsT t ]

	TWhere t1 vts
	 -> unions 
	 	[ freeVarsT t1
		, Set.unions $ map (freeVarsT . snd) vts ]
		
		\\ (Set.fromList $ map fst vts)
		
	TSum k ts
	 -> unions $ map freeVarsT ts

	TVar k v
	 -> Set.singleton v

	-- data
	TData v xs
	 -> unions
	 	[ fromList [v] 
		, unions $ map freeVarsT xs ]

	TFun t1 t2
	 -> unions
	 	[ freeVarsT t1 
		, freeVarsT t2 ]
	
	TFunEC	t1 t2 eff clo
	 -> unions 
	 	[ freeVarsT t1
		, freeVarsT t2 
		, freeVarsT eff
		, freeVarsT clo ]

	-- effect
	TEffect v ts
	 -> unions
	 	[ fromList [v]
		, unions $ map freeVarsT ts ]

	TPure		-> Set.empty

	-- closure
	TFree v t	-> freeVarsT t

	TEmpty		-> Set.empty
	
	
	-- class
 	TClass v ts	
	 -> unions
	 	[ fromList [v] 
		, unions $ map freeVarsT ts ]
	
	

	TKind k		-> Set.empty

	_ 	-> panic stage
		$ "freeVarsT: no match for " % show tt

	 
boundByS ::	Stmt	-> Set Var
boundByS	x
 = case x of
 	SBind v e
	 -> let
	    in	unions	[ fromList (maybeToList v) ]

	_ -> empty
			
		

varsBoundByG ::	Guard -> Set Var
varsBoundByG	g
 = case g of
--	GCase w		-> varsBoundByW w
	GExp  w x	-> varsBoundByW w
	

varsBoundByW ::	Pat	-> Set Var
varsBoundByW	ww
 = case ww of
 	WConst c	-> empty
	WCon   v fs	-> fromList $ map t3_2 fs
	
	






