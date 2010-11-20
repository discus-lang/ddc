{-# OPTIONS -fwarn-incomplete-patterns #-}

-- | Collect the free value/type variables from an expression,
--   along with their type/kinds.
module Core.Plate.FreeVarsXT
	( freeVarsXT
	, takeExpOfFree,  isFreeX
	, takeTypeOfFree, isFreeT
	, takeVarOfFree)
where
import DDC.Core.Exp
import DDC.Type
import DDC.Var
import DDC.Main.Error
import Data.Map			(delete, unions, union, empty, singleton)
import Util			hiding ((\\), delete, union)
import qualified Data.Map 	as Map
import qualified Data.Set	as Set

stage = "Core.Plate.FreeVarsXT"

-- | Holds a type or a kind.
data FreeXT
	= FreeX	Exp	-- ^ Contains an XVar, which also contains the var's type.
	| FreeT Type	-- ^ Contains a  TVar, which also contains the var's kind and optional more-than bound.
	deriving Show

takeExpOfFree ff
 = case ff of
	FreeX x	-> Just x
	_	-> Nothing

takeTypeOfFree ff
 = case ff of
	FreeT t	-> Just t
	_	-> Nothing

isFreeX ff	= isJust $ takeExpOfFree ff
isFreeT ff	= isJust $ takeTypeOfFree ff


-- | Take the real variable stashed in this `Free`, if any.	
takeVarOfFree :: FreeXT -> Maybe Var
takeVarOfFree free
 = case free of
	FreeX (XVar v _) 	-> Just v
	FreeT (TVar k u)	-> takeVarOfBound u
	_			-> Nothing


class FreeVarsXT a where
 freeVarsXT :: a -> Map Var FreeXT

-- Kind -------------------------------------------------------------------------------------------
instance FreeVarsXT Kind where
 freeVarsXT kk		= empty

-- Type -------------------------------------------------------------------------------------------
instance FreeVarsXT Type where
 freeVarsXT tt
	= Map.fromList
	$ map (\t -> case t of
			TVar _ b
			 | Just v <- takeVarOfBound b 	->  (v, FreeT t)
			_ -> panic stage $ "freeVarsXT: no match")
	$ filter isSomeTVar
	$ Set.toList
	$ freeTVars tt

-- Exp ---------------------------------------------------------------------------------------------
instance FreeVarsXT Exp where
 freeVarsXT xx
  = case xx of
	XNil		-> empty
	
 	XVar	v t	
	 -> unions
	 	[ singleton v $ FreeX xx
		, freeVarsXT t ]

	XLAM b k e
	 -> let Just v	= takeVarOfBind b
	    in  Map.delete v 
		  $ unions 
			[ freeVarsXT k
			, freeVarsXT e]

	XAPP x t
	 -> unions
	 	[ freeVarsXT x
		, freeVarsXT t ]

	XLam v t e eff clo	
	 -> delete v $ unions
	 	[ freeVarsXT t
		, freeVarsXT e ]

	XApp x1 x2
	 -> unions
	 	[ freeVarsXT x1
		, freeVarsXT x2 ]

	XTau t x
	 -> unions
	 	[ freeVarsXT t
		, freeVarsXT x ]

	XLocal v vs x
	 -> delete v 
		$ freeVarsXT x

	XDo xs
	 -> let	bound	= Set.toList $ Set.unions
			$ map boundByS xs
			
		free	= unions
			$ map freeVarsXT xs

	    in foldr delete free bound
		
	XMatch aa
	 -> unions $ map freeVarsXT aa
		
	XLit{}
	 -> empty
	
	XPrim m xs
	 -> unions
	 	[ freeVarsXT m
		, unions $ map freeVarsXT xs ]

	XPrimType t
	 -> freeVarsXT t

boundByS :: Stmt -> Set Var
boundByS x
 = case x of
 	SBind v e
	 -> Set.fromList (maybeToList v)


-- Prim --------------------------------------------------------------------------------------------
instance FreeVarsXT Prim where
 freeVarsXT m	= empty


-- Stmt --------------------------------------------------------------------------------------------
instance FreeVarsXT Stmt where
 freeVarsXT xx
  = case xx of
 	SBind v x
	 -> let bound	= maybeToList v
	    in	foldr delete (freeVarsXT x) bound


-- Alt  --------------------------------------------------------------------------------------------
instance FreeVarsXT Alt where
 freeVarsXT aa	
  = case aa of
 	AAlt gs x
	 -> let	bound	= Set.toList $ Set.unions 
			$ map varsBoundByG gs
			
		free	= unions 
		 	[ unions $ map freeVarsXT gs
			, freeVarsXT x ]

	    in	foldr delete free bound
				

varsBoundByG ::	Guard -> Set Var
varsBoundByG	g
 = case g of
	GExp  w x	-> varsBoundByW w


-- Guard -------------------------------------------------------------------------------------------
instance FreeVarsXT Guard where
 freeVarsXT gg
  = case gg of
	GExp  w x
	 -> let bound	= Set.toList
			$ varsBoundByW w
			
		free	= union (freeVarsXT x) (freeVarsXT w)

	    in	foldr delete free bound


varsBoundByW ::	Pat	-> Set Var
varsBoundByW	ww
 = case ww of
	WVar v		-> Set.singleton v
 	WLit{}	 	-> Set.empty
	WCon sp v fs	-> Set.fromList $ map t3_2 fs
	

-- Pat ---------------------------------------------------------------------------------------------
instance FreeVarsXT Pat where
 freeVarsXT pp
  = case pp of
	WVar v		-> empty
  	WLit sp c	-> empty
	WCon sp v lts	-> unions $ map (freeVarsXT . t3_3) lts


