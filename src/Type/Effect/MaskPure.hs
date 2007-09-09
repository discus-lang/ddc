
module Type.Effect.MaskPure
(
--	maskEsPureT
)

where

-----
import Util

import qualified Shared.Var	as Var
import Shared.Var		(NameSpace(..))

import Shared.VarBind		as Var

import Type.Exp
import Type.Util
import Type.Plate
import Type.Pretty



{-
maskEsPureT ::	Type -> Type
maskEsPureT	t
 = case t of
	TForall vks t
	 -> let t'	= maskEsPureT t
	    in	TForall vks t'

 	TFetters fs x
	 -> let
	 	pureVs	= catMap getPureVarsF fs
		fs'	= map (maskPureEF pureVs) fs
		
		x'	= scrubPureTFun pureVs x
				
            in 	TFetters fs' x'

	_ -> t
	


getPureVarsF :: Fetter -> [Var]
getPureVarsF	f
 = case f of
 	FClass v [TEffect (EVar eV)]
	 |  Var.bind v == Var.FPure
	 -> [eV]
	 
	 
	FClass v [TRegion (RVar rV)]
	 | Var.bind v == Var.FConst
	 -> [rV]
	 
	_ -> []
	


maskPureEF ::	[Var] -> Fetter	-> Fetter
maskPureEF	pureVs	 f
 = case f of
 	FEffect v (ESum es)
	 -> let	es'	= catMaybes 
	 		$ map (maskPureEs pureVs) es

   	    in	FEffect v (ESum es')
	    
	_ -> f
	

maskPureEs ::	[Var] -> Effect -> Maybe Effect
maskPureEs	pureVs	 e
 = case e of
 	EVar v
	 | elem v pureVs	-> Nothing

	ECon v [TRegion (RVar r)]
	 |   Var.name v == "Read"
	  && elem r pureVs	-> Nothing
	 
	_  			-> Just e


scrubPureTFun :: [Var] -> Type -> Type
scrubPureTFun	   pureVs   t
 = case t of
 	TFun t1 t2 eff clo
	 -> let
	 	eff'	= case eff of
			    EVar eV
			     | elem eV pureVs 	-> ENil
			    
			    _			-> eff
		
		t2'	= scrubPureTFun pureVs t2
		
	    in	TFun t1 t2' eff' clo

	_ -> t

-}





