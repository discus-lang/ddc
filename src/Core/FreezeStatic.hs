
module Core.FreezeStatic
(
	gatherStaticRegions
)

where

import Util

import qualified Data.Map 	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Shared.Var	as Var
import Shared.Var		(NameSpace(..))

import Core.Exp
import Core.Plate.FreeVars

gatherStaticRegions  :: Tree -> [Var]
gatherStaticRegions	tree	= []

{-
	= concat
	$ map gatherStaticRegionsP 
	$ tree

gatherStaticRegionsP ::	Top -> [Var]
-}

{-
gatherStaticRegionsP	p

	| PBind v t e		<- p
	, TCon vs xx		<- t
	= filter (\v -> Var.nameSpace v == NameRegion)
	$ Set.toList 
	$ freeVarsT t
	
	| otherwise
	= []

-}
