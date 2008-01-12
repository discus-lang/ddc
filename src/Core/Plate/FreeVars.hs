{-# OPTIONS -fwarn-incomplete-patterns #-}

module Core.Plate.FreeVars
	( freeVars
	, varsBoundByG
	, varsBoundByW )

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

class FreeVars a where
 freeVars :: a -> Set Var
 
instance FreeVars a => FreeVars [a] where
 freeVars xx	= Set.unions $ map freeVars xx
  
-- Exp ---------------------------------------------------------------------------------------------
instance FreeVars Exp where
 freeVars xx
  = case xx of
	XNothing	-> empty
	XNil		-> empty
	
	XAnnot	n x	
	 -> freeVars x

 	XVar	v t	
	 -> unions
	 	[ singleton v
		, freeVars t ]

	XLAM v k e
	 -> unions 
	 	[ freeVars k
		, freeVars e ]
		\\ Set.singleton (varOfBind v)

	XAPP x t
	 -> unions
	 	[ freeVars x
		, freeVars t ]

	XLam v t e eff clo	
	 -> unions
	 	[ freeVars t
		, freeVars e ]
		\\ fromList [v]

	XApp x1 x2 eff
	 -> unions
	 	[ freeVars x1
		, freeVars x2 
		, freeVars eff ]

	XTau t x
	 -> unions
	 	[ freeVars t
		, freeVars x ]

	XTet vts x
	 -> unions
	 	[ freeVars x
		, Set.unions $ map (freeVars . snd) vts]
		
		\\ (Set.fromList $ map fst vts)

	XLocal v vs x
	 -> freeVars x \\ fromList [v]

	XDo xs
	 -> let
	 	bound	= unions
			$ map boundByS xs
			
		free	= unions
			 $ map freeVars xs

	    in free \\ bound
		
	XMatch aa
	 -> unions $ map freeVars aa
		
	XConst c t	
	 -> freeVars t

	XLifted v vsFree
	 -> fromList vsFree
	
	XPrim m xs
	 -> unions
	 	[ freeVars m
		, unions $ map freeVars xs ]
	
	XProject x j
	 ->	freeVars x


	-- atoms
	XAtom v xs
	 -> unions
	 	[ fromList [v]
		, unions $ map freeVars xs ]

	-- intermediate
	XType t
	 -> freeVars t

	_ 	-> panic stage
		$ "freeVarsX: no match for " % show xx


boundByS ::	Stmt	-> Set Var
boundByS	x
 = case x of
 	SBind v e
	 -> let
	    in	unions	[ fromList (maybeToList v) ]


-- Prim --------------------------------------------------------------------------------------------
instance FreeVars Prim where
 freeVars m
  = case m of

	MSuspend v	-> singleton v
	MForce 		-> empty

	MBox t1 t2
	 -> unions
	 	[ freeVars t1
		, freeVars t2 ]

	MUnbox t1 t2 
	 -> unions
	 	[ freeVars t1
		, freeVars t2 ]

	MTailCall 	-> empty
	MCall 	 	-> empty
	MCallApp  i	-> empty
	MApply 		-> empty
	MCurry 	 i	-> empty
	MFun 		-> empty	


-- Stmt --------------------------------------------------------------------------------------------
instance FreeVars Stmt where
 freeVars xx
  = case xx of
 	SBind v x
	 -> unions 
	 	[ freeVars x ]
		\\ fromList (maybeToList v)


-- Alt  --------------------------------------------------------------------------------------------
instance FreeVars Alt where
 freeVars aa	
  = case aa of
 	AAlt gs x
	 -> let
	 	bound	= unions 
			$ map varsBoundByG gs
			
		free	= unions 
		 	[ unions $ map freeVars gs
			, freeVars x ]
	    in	free \\ bound
				

varsBoundByG ::	Guard -> Set Var
varsBoundByG	g
 = case g of
	GExp  w x	-> varsBoundByW w


-- Guard -------------------------------------------------------------------------------------------
instance FreeVars Guard where
 freeVars gg
  = case gg of
	GExp  w x
	 -> (Set.union (freeVars x) (freeVars w))
	 	\\ varsBoundByW w

varsBoundByW ::	Pat	-> Set Var
varsBoundByW	ww
 = case ww of
 	WConst c t	-> empty
	WCon   v fs	-> fromList $ map t3_2 fs
	

-- Pat ---------------------------------------------------------------------------------------------
instance FreeVars Pat where
 freeVars pp
  = case pp of
  	WConst	c t	-> freeVars t
	WCon 	v lts	-> Set.unions $ map (freeVars . t3_3) lts


-- Type --------------------------------------------------------------------------------------------
instance FreeVars Type where
 freeVars tt
  = case tt of
	TNil		-> empty

 	TForall v k t	
	 -> unions
	 	[ freeVars k
		, freeVars t]
		 \\ (Set.singleton $ varOfBind v)

	TContext k t
	 -> unions
	 	[ freeVars k
		, freeVars t ]

	TFetters t1 fs
	 -> unions 
	 	[ freeVars t1
		, freeVars fs ]
		\\ (Set.fromList $ map varOfFetter fs)
		
	TSum k ts
	 -> unions $ map freeVars ts

	TMask k t1 t2
	 -> unions
	 	[ freeVars t1
		, freeVars t2 ]

	TVar k v	-> Set.singleton v
	
	TBot k		-> Set.empty
	TTop k		-> Set.empty

	-- data
	TData v xs
	 -> unions
	 	[ fromList [v] 
		, unions $ map freeVars xs ]

	TFun t1 t2
	 -> unions
	 	[ freeVars t1 
		, freeVars t2 ]
	
	TFunEC	t1 t2 eff clo
	 -> unions 
	 	[ freeVars t1
		, freeVars t2 
		, freeVars eff
		, freeVars clo ]

	-- effect
	TEffect v ts
	 -> unions
	 	[ fromList [v]
		, unions $ map freeVars ts ]

	-- closure
	TFree v t	-> freeVars t
	TTag v		-> Set.empty
	
	-- class
 	TClass v ts	
	 -> unions
	 	[ Set.singleton v
		, unions $ map freeVars ts ]
	
	TPurify eff cls
	 -> unions
	 	[ freeVars eff
		, freeVars cls ]
	
	TPurifyJoin ts
	 -> unions
	 	$ map freeVars ts
	
	TWitJoin ts
	 -> unions $ map freeVars ts

	_ 	-> panic stage
		$ "freeVarsT: no match for " % show tt

-- Fetter ------------------------------------------------------------------------------------------
instance FreeVars Fetter where
 freeVars ff
  = case ff of
  	FWhere v t	-> freeVars t \\ singleton v
	FMore  v t	-> freeVars t


-- Kind --------------------------------------------------------------------------------------------
instance FreeVars Kind where
 freeVars kk
  = case kk of
 	KNil		-> Set.empty
	KData		-> Set.empty
	KRegion		-> Set.empty
	KEffect		-> Set.empty
	KClosure	-> Set.empty

	KClass v ts
	 -> unions
	 	[ Set.singleton v
		, unions $ map freeVars ts ]
		
	KFun k1 k2
	 -> unions
	 	[ freeVars k1 
		, freeVars k2 ]

