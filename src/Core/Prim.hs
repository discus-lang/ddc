
-- Core.Prim
--	Find direct uses of primitive functions and replace them by XPrim nodes.
--
module Core.Prim
(
	primTree
)


where

import Util
import Core.Exp
import Core.Util
import Core.Plate.Trans

import qualified Shared.Var	as Var

primTree :: Tree -> Tree
primTree tree
	= transformS primS tree
	
primS ss
	| SBind mV (XTau t x)	<- ss
 	, (args, eff)		<- stripArgsEffs x
	, (XVar v _ : args)	<- args

	, argsV			<- catMaybes 
				$ map (\a -> case a of 
						XType{} -> Nothing
						_	-> Just a)
				$ args

	, elem (Var.name v) primFuns
	= SBind mV (XPrim (MFun v t) argsV eff)
	
	| otherwise		= ss
	

stripArgsEffs xx
 = let	parts	= flattenAppsE xx
 	
	(args, effss)
		= unzip
		$ map (\p -> case p of
				XAppFP x mEff	-> (x, mEff))
		$ parts

	eff	= makeTSum KEffect
		$ catMaybes effss
	
    in	(args, eff)
				

primFuns = 
	[ "primInt32U_add"
	, "primInt32U_sub"
	, "primInt32U_mul"
	, "primInt32U_div"
	, "primInt32U_mod"
	, "primInt32U_eq"
	, "primInt32U_neq"
	, "primInt32U_gt"
	, "primInt32U_ge"
	, "primInt32U_lt"
	, "primInt32U_le" 

	, "primFloat32U_add"
	, "primFloat32U_sub"
	, "primFloat32U_mul"
	, "primFloat32U_div"
	, "primFloat32U_mod"
	, "primFloat32U_eq"
	, "primFloat32U_neq"
	, "primFloat32U_gt"
	, "primFloat32U_ge"
	, "primFloat32U_lt"
	, "primFloat32U_le" ]
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
