
-- | Rename variables in a core tree.
module Core.Util.Rename 
	(renameBindersTree
	, renameBindersX
	, RenameS(..))
where
import Core.Exp
import Core.Plate.Trans
import Shared.Pretty
import Util
import DDC.Var.VarId
import qualified Shared.Var	as Var
import qualified Data.Map	as Map
import Shared.Var		(Var)


-- | Rename variables in this tree.
renameBindersTree :: Tree -> RenameM Tree
renameBindersTree tree
 = 	transZM
 		transTableId
		{ transV_bind	= renameV_bind
		, transV_free	= renameV_free }
		tree	


-- | Renamer state.
type RenameM = State RenameS
data RenameS
 =	RenameS
 	{ sVarGen	:: VarId
	, sVarMap	:: Map Var Var }


-- | Rename binders in this expression
renameBindersX :: Exp -> RenameM Exp
renameBindersX x
 = 	transZM	transTableId
 		{ transV_bind	= renameV_bind
		, transV_free	= renameV_free }
		x


-- | Rename bound occurrences of variables.
renameV_free :: Var -> RenameM Var
renameV_free v
 = do	varMap	<- gets sVarMap
 	case Map.lookup v varMap of
	 Nothing	-> return v
	 Just v'	-> return v'

	
-- | Rename binding occurrences of variable.
renameV_bind :: Var -> RenameM Var
renameV_bind v
 = do	varMap 	<- gets sVarMap

	-- update the var gen
 	varGen	<- gets sVarGen
	modify (\s -> s { sVarGen = incVarId varGen})

	-- make the new var and add it to the map
	let v'	= v 	{ Var.name = pprStrPlain varGen
			, Var.varId = varGen }

	modify (\s -> s { sVarMap = Map.insert v v' (sVarMap s)})
	
	return v'
