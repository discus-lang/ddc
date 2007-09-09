
module Core.Optimise.Inline
	( inlineTree )
	
where

import Util
import Data.Map			(Map)
import qualified Data.Map	as Map

import Shared.Var		(Var)
import qualified Shared.Var	as Var
import Shared.VarBind

import Core.Exp
import Core.Inline
import Core.Util.Beta

inlineTree :: Tree -> Tree -> [Var] -> Tree
inlineTree sTree hTree inlineVars
 = let	binds		= gatherBinds
 				(map Var.name inlineVars)
				sTree

	sTree'		= evalState (inlineBindsTree_rename binds sTree) (XBind "foo" 0)
  	sTreeBeta	= betaTree $ betaTree sTree'
  
   in	sTreeBeta

gatherBinds :: [String] -> Tree -> Map Var Exp
gatherBinds names tree
	= foldr (gatherBind names) Map.empty tree 

gatherBind :: [String] -> Top -> Map Var Exp -> Map Var Exp
gatherBind names bind@(PBind v x) binds
	| elem (Var.name v) names
	= Map.insert v x binds
	
gatherBind _ _ binds 
	= binds








