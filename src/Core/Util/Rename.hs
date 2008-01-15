
module Core.Util.Rename 
	(renameBindersTree
	, renameBindersX
	, RenameS(..))
where

import Util
import Core.Exp

import Core.Plate.Trans

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Shared.Var	as Var
import Shared.Var		(Var)
import Shared.VarBind		(VarBind, incVarBind)

-----
-- Rename binders
--
renameBindersTree :: Tree -> RenameM Tree
renameBindersTree tree
 = 	transZM
 		transTableId
		{ transV_bind	= renameV_bind
		, transV_free	= renameV_free }
		tree	


type RenameM = State RenameS
data RenameS
 =	RenameS
 	{ sVarGen	:: VarBind
	, sVarMap	:: Map Var Var }

renameBindersX :: Exp -> RenameM Exp
renameBindersX x
 = 	transZM	transTableId
 		{ transV_bind	= renameV_bind
		, transV_free	= renameV_free }
		x


renameV_free v
 = do	varMap	<- gets sVarMap
 	case Map.lookup v varMap of
	 Nothing	-> return v
	 Just v'	-> return v'
 	
renameV_bind v
 = do	varMap 	<- gets sVarMap

	-- update the var gen
 	varGen	<- gets sVarGen
	modify (\s -> s { sVarGen = incVarBind varGen})

	-- make the new var and add it to the map
	let v'	= v { Var.name = pprStr varGen, Var.bind = varGen }
	modify (\s -> s { sVarMap = Map.insert v v' (sVarMap s)})
	
	return v'
