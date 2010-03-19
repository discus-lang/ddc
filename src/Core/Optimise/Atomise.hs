
module Core.Optimise.Atomise
	( atomiseTree )
where
import Core.Exp
import Core.Plate.Trans
import Type.Exp
import qualified Shared.Var		as Var
import qualified Shared.VarBind	as Var

import	Util

-- | Rewrite references to constant data constructors of arity 0 to
--	use a single shared atomic value.
atomiseTree 
	:: Glob 	-- header glob
	-> Glob		-- source glob
	-> Glob

atomiseTree cSource cHeader 
 = cSource

{- transformX atomiseX cSource
 	
atomiseX :: Exp -> Exp

atomiseX xx
 	| XPrim MCall ts@[XVar v tV, _, XType (TVar kRegion r)]	
					<- xx
	, elem (Var.bind v) atomBinds
	, isConst r
	= XAtom v ts

	| XPrim MCall [XVar v tV]	<- xx
	, Var.bind v == Var.VUnit
	= XAtom v []
	 
	| otherwise
	= xx

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
