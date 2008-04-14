
module Core.Util.Effect
	( crushEffsT )
where


import Util
import Core.Exp
import Core.Plate.Trans
import Core.Util.Bits
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
	= makeTSum KEffect $ flattenTSum tt

 	| TEffect v ts		<- tt
	, elem v [Var.primReadT, Var.primWriteT]
	, bits			<- catMap slurpDataRT ts
	= makeTSum KEffect
		$ map (\bit -> case (Var.bind v, bit) of 
				(Var.EReadT,  TVar KRegion v)	-> TEffect Var.primRead  [bit]
				(Var.EReadT,  TVar KData   v)	-> TEffect Var.primReadT [bit]
				(Var.EWriteT, TVar KRegion v)	-> TEffect Var.primWrite [bit]
				(Var.EWriteT, TVar KData   v)	-> TEffect Var.primWriteT [bit])
		$ bits
	
	| otherwise
	= tt

-- | Slurp out components of this type which are interesting to !ReadT \/ !WriteT
--	duplicated in Core.Crush
slurpDataRT :: Type -> [Type]
slurpDataRT tt
	| TFun{}		<- tt	= []

	| TFunEC{}		<- tt	= []

	| TApp{}		<- tt
	, Just (v, k, ts)	<- takeTData tt
	= catMap slurpDataRT ts
	
	| TFun{}		<- tt	= []
	
 	| TFunEC{}		<- tt	= []
	
	| TVar KRegion _ 	<- tt	= [tt]
	| TVar KData   _	<- tt	= [tt]
	| TVar _  _		<- tt	= []
	
	| TCon{}		<- tt	= []
	
	| otherwise			
	= panic stage
		$  "slurpDataRT: no match for " % tt % "\n"




