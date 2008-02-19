
module Type.Util.Kind
	(defaultKindV)
where

import Type.Exp

import Shared.Var		(NameSpace(..))
import qualified Shared.Var as Var


defaultKindV ::	Var	-> Kind
defaultKindV	v
 = case Var.nameSpace v of
 	NameType	-> KData
	NameRegion	-> KRegion
	NameEffect	-> KEffect
	NameClosure	-> KClosure
	
	
