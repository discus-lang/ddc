-----------------------
-- maskFreshT
--	Mask effects on fresh regions.
--
-- Example:
--		f :: forall %r2. C %r1 -(!e1)> C %r2
--	 	  :- !e = {!Read %r1, !Read %r2, !Write %r2}
--
--	In the type for f, region %r2 is fresh.
--	The caller doesn't care about the effects used to create r2, so 
--		we can mask them from the type.
--
--	Can't mask if the region is present in other args, or in 
--		the environment of any of the functions.
--
--		f :: forall %r2. C{%r1} D{%r2} -e> D{%r2}
--	 	  :- e = {Read %r2}
--


module Type.Effect.MaskFresh
	( 
--	maskEsFreshT 
	)

where

-----
import Util
import qualified Shared.Var	as Var
import Shared.Var		(NameSpace(..))

import Type.Exp
import Type.Pretty
import Type.State
import Type.Util.Bits
import Type.Plate

import qualified Debug.Trace	as Debug

-----
stage	= "Type.Effect.MaskFresh"
debug	= True
trace s	= when debug $ traceM s


maskEsFreshT ::	Type -> Type
maskEsFreshT t = t

{-
maskEsFreshT	t@(TForall vks t1@(TFetters fs x@(TFun{})))
 = let
	tFlat@(TFunF parts)
			= flattenFunT x

	-- Gather up the regions being quantified by the forall.
	-- 	Can only mask effects on fresh regions 
	--
	quantRs		= filter (\v -> Var.nameSpace v == NameRegion)
			$ map fst vks
		
	-- Collect all the TCons referenced by the arguments of the function.
	argCons		= concat
			$ [collectTConsT t 
					| (t, e, n) 
					<- init parts]

	-- Collect all the TCons referenced by the environment of the function.
	envCons		= concat
			$ [collectTConsT x
				| CFreeT v x
				<- concat [ cs | FClosure _ (CSum cs) <- fs]]
					

	-- If an effect access a region in the argument list or
	--	from the function environment then it cannot be masked.
	staticRs	= concat
			$ map takeTConRegions 
			$ (argCons ++ envCons)
				
			
	-- The maskable regions are the ones that are quantified
	--	by the forall, 
	--		and are NOT in the argument list.
	--		and are NOT in the function environments
	--
	maskableRs 	= filter (\v -> not $ elem v staticRs)
			$ quantRs
	
	fs'		= catMaybes
			$ map (maskFreshF maskableRs) fs

	in	TForall vks (TFetters fs' x)

maskEsFreshT	t	= t

	
maskFreshF :: 	[Var] -> Fetter -> Maybe Fetter
maskFreshF	maskRs   f
 = case f of
 	FEffect e eff
	 -> let	eff'	= maskFreshE maskRs eff
	    in	Just $ FEffect e eff'

	FClass v [TRegion r]
	 |  Var.bind v == Var.FMutable
	 && maskFreshR maskRs r
	 -> Nothing
	 
	 |  otherwise
	 -> Just f
	    
	_ -> Just f
	
	

maskFreshE ::	[Var] -> Effect -> Effect
maskFreshE	freshRs  eff
 = case eff of
 	ESum es
	 -> let	es'	= catMaybes 
	 		$ map (maskFreshE' freshRs) es
	    in	ESum es'
	    
	ENil
	 ->	ENil


maskFreshE' maskRs e
	| ECon v ts		<- e
	, elem (Var.name v) ["Read", "Write"]
	, and	$ map (maskFreshET maskRs) ts
	= Nothing
	
	| otherwise
	= Just e

maskFreshET ::	[Var] -> Type   -> Bool
maskFreshET	mashRs   t
 = case t of
 	TRegion r	-> maskFreshR mashRs r
	_		-> False
	
maskFreshR ::	[Var] -> Region -> Bool
maskFreshR	maskRs   r
 = case r of
 	RClass cidR	-> False
	RVar   v	-> elem v maskRs
	



-}
