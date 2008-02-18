
module Type.Util.Kind
(
	addDefaultKinds,
	defaultKindV
)

where

-----
import Util

import qualified Shared.Var as Var
import Shared.Var		(NameSpace(..))

import Type.Exp
import Type.Plate

-----------------------
-- addDefaultKinds
--
addDefaultKinds :: 	Type -> Type
addDefaultKinds	tt
	= transformT addDefaultKinds' tt

addDefaultKinds' tt
 = case tt of
 	TForall vsKinds t
	 -> let vsKinds'	= map (\v -> (v, defaultKindV v)) $ map fst vsKinds
	    in  TForall vsKinds' t
		
	_ -> tt


-----------------------
-- defaultKindV
--
defaultKindV ::	Var	-> Kind
defaultKindV	v
 = case Var.nameSpace v of
 	NameType	-> KData
	NameRegion	-> KRegion
	NameEffect	-> KEffect
	NameClosure	-> KClosure
	
	
