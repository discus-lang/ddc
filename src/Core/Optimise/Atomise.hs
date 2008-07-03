
module Core.Optimise.Atomise
	( atomiseTree )
where

import	Core.Exp

import	qualified Shared.Var		as Var
import	qualified Shared.VarBind	as Var

import	Util

-- Replace constant data constructors with references to a single
atomiseTree 
	:: Tree 	-- source tree
	-> Tree		-- header tree
	-> Tree

atomiseTree cSource cHeader 
 = cSource
 
 {-
 transformX atomiseX cSource
 
	
atomiseX 
	:: Exp -> Exp

atomiseX xx
 	| XPrim MCall ts@[XVar v tV, _, XType (TVar KRegion r)]	
					<- xx

	, elem (Var.bind v) atomBinds
	, isConst r
	= return $ XAtom v ts

	| XPrim MCall [XVar v tV]	<- xx
	, Var.bind v == Var.VUnit
	= return $ XAtom v []
	 
	| otherwise
	= return xx

atomBinds
	= [Var.VNil, Var.VFalse, Var.VTrue]
	

isConst ::  Var -> Bool
isConst  v
--	= Debug.trace ("fs = " ++ (show $ Map.keys $ boundFs table))
	= isConst'  v

isConst'  v
	| Just fs	<- Nothing -- Map.lookup v $ boundFs table
	, or $ map (\(TClass v _) -> Var.bind v == Var.FConst) fs
	= True
	
	| otherwise
	= False
-}
