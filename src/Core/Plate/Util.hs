
module Core.Plate.Util
(
	eraseModuleTree
)

where

import qualified Shared.Var		as Var
import Shared.Var			(Module(..))

import Core.Exp
import Core.Util
import qualified Core.Plate.Trans	as Trans

eraseModuleTree :: Tree -> Tree
eraseModuleTree tree
	= Trans.transformV (\v -> v { Var.nameModule = ModuleNil }) tree
	
	
