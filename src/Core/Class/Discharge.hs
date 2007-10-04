
module Core.Class.Discharge
	( dischargeC )
where

import Core.Exp
import Core.Util.Unify

import Shared.Error
import qualified Shared.Var	as Var
import Util

-----
stage	= "Core.Class.Discharge"

-----
dischargeC :: Class -> Maybe Bool

dischargeC (TClass cls tt)

	-- Shape constraints can be discharged if all their args have
	--	can be unified without creating more data constraints.
	| Var.FShape i		<- Var.bind cls
	, (t : ts)		<- tt
	, Just constraintss	<- sequence $ map (unifyT2 t) ts 
	, (tsL, tsR)		<- unzip $ concat constraintss
	= Just $ and 
	$ map (\x -> case x of
			TData{}		-> False
			TFunEC{}	-> False)
	$ tsL ++ tsR
	
	-- can't discharge
	| otherwise
	= Just False
	
dischargeC tt
	= freakout stage 
		("dischargeC: can't discharge class constraint " % tt)
		Nothing	
