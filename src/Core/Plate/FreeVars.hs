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
import Type.Util.Bits		(varOfBind)

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
		
	XLit{}
	 -> empty

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
	MBox		-> empty
	MUnbox	 	-> empty
	MTailCall 	-> empty
	MCall 	 	-> empty
	MCallApp  i	-> empty
	MApply 		-> empty
	MCurry 	 i	-> empty
	MFun 		-> empty	
	MOp{}		-> empty


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
	WVar v		-> Set.singleton v
 	WLit{}	 	-> empty
	WCon	v fs	-> fromList $ map t3_2 fs
	

-- Pat ---------------------------------------------------------------------------------------------
instance FreeVars Pat where
 freeVars pp
  = case pp of
	WVar v		-> empty
  	WLit c		-> empty
	WCon v lts	-> Set.unions $ map (freeVars . t3_3) lts


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
		\\ Set.fromList [v | FWhere (TVar _ v) _ <- fs]
		
	TSum k ts
	 -> unions $ map freeVars ts

	TMask k t1 t2
	 -> unions
	 	[ freeVars t1
		, freeVars t2 ]

	TVar k v	-> Set.singleton v
	
	TVarMore k v t
	 -> unions
	 	[ Set.singleton v
		, freeVars t]
	
	TCon con
	 -> freeVars con
	
	TBot k		-> Set.empty
	TTop k		-> Set.empty

	TApp t1 t2
	 -> unions
		[ freeVars t1
		, freeVars t2 ]

	-- effect
	TEffect v ts
	 -> unions
	 	[ fromList [v]
		, unions $ map freeVars ts ]

	-- closure
	TFree v t	-> freeVars t
	TTag v		-> Set.empty
	
{-	TPurify eff cls
	 -> unions
	 	[ freeVars eff
		, freeVars cls ]
-}
--	TPurifyJoin ts
--	 -> unions
--	 	$ map freeVars ts
	
	TWitJoin ts
	 -> unions $ map freeVars ts

	_ 	-> panic stage
		$ "freeVarsT: no match for " % show tt

-- TyCon -------------------------------------------------------------------------------------------
instance FreeVars TyCon where
 freeVars tt
  = case tt of
  	TyConFun{}			-> Set.empty
	TyConData  { tyConName }	-> Set.singleton tyConName
	TyConClass { }			-> Set.empty
--	TyConPurify{}			-> Set.empty
--	TyConPureJoin{}			-> Set.empty
	

-- Fetter ------------------------------------------------------------------------------------------
instance FreeVars Fetter where
 freeVars ff
  = case ff of
  	FWhere (TVar k v) t	-> freeVars t \\ singleton v
	FMore  v t		-> freeVars t
	_	-> panic stage
		$ "freeVars[Fetter]: no match for " % ff


-- Kind --------------------------------------------------------------------------------------------
instance FreeVars Kind where
 freeVars kk
  = case kk of
 	KNil		-> Set.empty

	KForall k1 k2
	 -> unions
	 	[ freeVars k1
		, freeVars k2 ]

	KFun k1 k2
	 -> unions
	 	[ freeVars k1 
		, freeVars k2 ]


	KValue		-> Set.empty
	KRegion		-> Set.empty
	KEffect		-> Set.empty
	KClosure	-> Set.empty

	KClass tc ts
	 -> unions
	 	[ unions $ map freeVars ts ]
		
	KWitJoin ks
	 -> freeVars ks

	_ 	-> panic stage
		$ "freeVars[Kind]: no match for " % kk
		
		
