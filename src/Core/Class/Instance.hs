
module Core.Class.Instance
(
	callInstances
)

where

import Util

import qualified Debug.Trace	as Debug
import qualified Shared.Var	as Var
import Shared.Var		(Var)

import Core.Exp


callInstances :: Tree -> Tree
callInstances	 ps
 = let	aliases	= catMaybes $ map slurpAliasP ps
	
   in	Debug.trace (pprStr $ "aliases = " % aliases) $ ps



slurpAliasP ::	Top -> Maybe (Var, Var)
slurpAliasP	p
 = case p of
 	PBind v s
	 -> let
	 	Just (Var.IAlias v2)
			= find ((=@=) Var.IAlias{})
			$ Var.info v
			
	    in	Just (v, v2)
		
	_ -> Nothing
		
