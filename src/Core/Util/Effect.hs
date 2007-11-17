
module Core.Util.Effect
	( crushEffsT )
where


import Util
import Core.Exp
import Core.Plate.Trans
import Core.Bits
import Core.Pretty
import Shared.Error

import qualified Shared.VarPrim	as Var
import qualified Shared.Var	as Var

stage	= "Core.Util.Effect"
-----

-- | Try and crush out 
--	EReadT, EWriteT, EReadH effects in this type

crushEffsT :: Type -> Type
crushEffsT tt	= transformT crushEffs tt

crushEffs tt
	| TSum KEffect _	<- tt
	= makeSumT KEffect $ crushSumT tt

 	| TEffect v ts		<- tt
	, v == Var.primReadT
	, bits			<- catMap slurpDataRT ts
	= makeSumT KEffect
		$ map (\bit -> case (Var.bind v, bit) of 
				(Var.EReadT, TVar KRegion v)	-> TEffect Var.primRead  [bit]
				(Var.EReadT, TVar KData   v)	-> TEffect Var.primReadT [bit])
		$ bits
	
	| otherwise
	= tt

-- | Slurp out components of this type which are interesting to !ReadT \/ !WriteT
--	duplicated in Core.Crush
slurpDataRT :: Type -> [Type]
slurpDataRT tt
 = case tt of
	TFun{}			-> []
 	TData v ts		-> catMap slurpDataRT ts

	TVar KRegion _		-> [tt]
	TVar KData   _		-> [tt]
	TVar _  _		-> []
	
	_ 	-> panic stage
		$  "slurpDataRT: no match for " % tt % "\n"




