
module Type.Effect.MaskLocal
	(
--	maskEsLocalT
	)

where

-----
import Util

import qualified Shared.Var	as Var
import Shared.Var		(NameSpace (..))

import Type.Exp
import Type.Plate
import Type.Util

-----------------------
-- maskLocal
--	Mask effects on local regions.
--	
--	At generalisation time, if a region is not present in the type
--	or environment of a function then is local to that function
--	and all effects involving that region are masked.
--
{-
maskEsLocalT ::	Monad m
	   =>	Type -> m Type

maskEsLocalT	t
 = let	visT		= visRegions t
	visE		= visEffects t
   in	maskEsLocalT' (visT ++ visE) t
   
			
maskEsLocalT'	visR	 t
 = case t of
	TForall vks t
	 -> do
	 	t'		<- maskEsLocalT' visR t

		return		$ TForall vks t'

	TFetters fs t
	 -> do
	 	let fs'		= map (maskF visR) fs
		return		$ TFetters fs' t

	_ ->	return t


maskF ::	[Var] -> Fetter -> Fetter
maskF		env	f
 = case f of
 	FEffect e eff
	 -> let	eff'	= maskE env eff
	    in	FEffect e eff'
	 	
	_ -> f


maskE :: 	[Var] -> Effect -> Effect
maskE		env	e
	| ESum es		<- e
	= let	es'	= catMaybes $ map (maskE' env) es
	  in	ESum $ flattenEs (ESum es')

	| otherwise
	= e
	
maskE'	env e

	| ECon v [TRegion (RVar r)]	<- e
	, elem (Var.name v) ["Read", "Write"]
	, not $ elem r env
	= Nothing
	
	| EVar v		<- e
	, not $ elem v env
	= Nothing
	
	| otherwise
	= Just e




-----------------------
-- visRegions
--	Collect the list of visible regions from the type sig. 
--	We can't just call freeVarsT, because we don't want to get
--		region vars present in the effect portion of the type.
--
visRegions :: 	Type -> [Var]
visRegions	t
	= catMaybes
	$ concat
	$ map (\(TCon v ts) -> map visRsTCon ts)
	$ collectTConsT t
	
visRsTCon t 
 = case t of
 	TRegion (RVar v)	-> Just v
	_			-> Nothing 




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
