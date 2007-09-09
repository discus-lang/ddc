
module Core.Optimise.MaskEffs
(
--	maskEffsTree
)

where

import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Shared.Var	as Var
import qualified Shared.VarBind	as Var
import Shared.VarPrim

import Core.Exp
import Core.Util
import Core.Util.Slurp

import Core.Plate.Walk

{-
type MaskM	= State ()

maskEffsTree :: Tree -> Tree
maskEffsTree	tree
	= evalState
		(walkZM walkTableId 
			{ transX	= maskEffsX }
			tree)
			
		()
		
-----
maskEffsX 
	:: WalkTable MaskM
	-> Exp -> MaskM Exp
	
maskEffsX table xx
 = case xx of
 	XApp x1 x2 eff
	 -> return 
	 	$ XApp x1 x2 
	 	$ maskEff table eff

	XPrim m xx eff
	 -> return	
	 	$ XPrim m xx
		$ maskEff table eff
		
	_ ->	return xx
		
maskEff table eff
	=  makeSumT KEffect
	$  catMaybes 
 	$  map (maskE (boundFs table))
	$  crushEffs eff
		

-----
maskE 	:: Map Var [Class]
	-> Effect -> Maybe Effect
	
maskE	boundFs ee
	| TEffect v [r@(TVar KRegion rV)]	<- ee
	, v == primRead
	, Just fs		<- Map.lookup rV boundFs
	, elem (TClass primConst [r]) fs
	= Nothing
	
	| otherwise
	= Just ee

-}
