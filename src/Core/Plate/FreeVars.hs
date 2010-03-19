{-# OPTIONS -fwarn-incomplete-patterns #-}

-- | Take sets of free variables by core expressions.
module Core.Plate.FreeVars
	( freeVars
	, varsBoundByG
	, varsBoundByW )
where
import Core.Exp
import Type.Util.Bits		(varOfBind)
import Type.Pretty
import Shared.Var		(Var, NameSpace(..))
import Shared.Error
import Shared.FreeVars
import qualified Shared.Var	as Var
import qualified Data.Set as Set
import Data.Set			(Set, (\\), unions, fromList, empty, singleton)
import Util			hiding ((\\))

-----
stage	= "Core.Plate.FreeVars"

 
-- Exp ---------------------------------------------------------------------------------------------
instance FreeVars Exp where
 freeVars xx
  = case xx of
	XNil		-> empty
	
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
	WCon sp v fs	-> fromList $ map t3_2 fs
	

-- Pat ---------------------------------------------------------------------------------------------
instance FreeVars Pat where
 freeVars pp
  = case pp of
	WVar v		-> empty
  	WLit sp c	-> empty
	WCon sp v lts	-> Set.unions $ map (freeVars . t3_3) lts

		
		
