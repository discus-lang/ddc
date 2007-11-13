
module Type.Effect.MaskLocal
	( maskEsLocalT )

where

-----
import Util

import qualified Shared.Var	as Var
import Shared.Var		(NameSpace (..))

import Type.Exp
import Type.Plate
import Type.Util

-- | Mask effects on local regions.
--	
-- At generalisation time, if a region is not present in the type or closure of a
-- function then is local to that function and all effects involving that region 
-- can be erased from the type.
--
maskEsLocalT :: Type -> Type
maskEsLocalT	t
 = let	visT		= visibleRs t
   in	maskEsLocalT' visT t
   
			
maskEsLocalT'	visR tt
 = case tt of
	TForall  vks t1		-> TForall vks (maskEsLocalT' visR t1)
	TFetters fs  t1		-> TFetters (map (maskF visR) fs) t1
	_ 			-> tt


-- | Erase read effects to regions not in thie list.
maskF :: [Var] -> Fetter -> Fetter
maskF	visR	(FLet t1 t2)
	| kindOfType t1 == KEffect
	= FLet t1 (maskE visR t2)
	
maskF	visR	f	= f


-- | Erase read effects to regions not in this list.
maskE :: [Var] -> Effect -> Effect
maskE	 env	eff
	| TSum KEffect es <- eff
	= makeTSum KEffect $ catMaybes $ map (maskE' env) es

	| otherwise
	= eff
	
maskE'	env eff

	| TEffect v [TVar KRegion r]	<- eff
	, elem (Var.name v ) ["Read", "Write"]
	, not $ elem r env
	= Nothing
	
	| otherwise
	= Just eff




-----------------------
-- visRegions
--	Collect the list of visible regions from the type sig. 
--	We can't just call freeVarsT, because we don't want to get
--		region vars present in the effect portion of the type.
--
visibleRs :: Type -> [Var]
visibleRs tt
	= catMaybes
	$ concat
	$ map (\(TData v ts) -> map visibleRsTCon ts)
	$ collectTConsT tt
	
visibleRsTCon t 
 = case t of
 	TVar KRegion v	-> Just v
	_		-> Nothing 



{-
-----------------------
-- visEffects
--	Collect the list of effect variables present in higher order terms.
--
visEffects :: Type	-> [Var]
visEffects    t
 = case t of
	TForall vks x 	-> visEffects2 x
	x		-> visEffects2 x
	
visEffects2 t
 = case t of
 	TFetters fs x	-> visEffects3 x
	x		-> visEffects3 x
	
visEffects3 t
 = let	vs	= filter (\v -> Var.nameSpace v == NameEffect)
		$ freeVarsT t
   in	vs	

-}
