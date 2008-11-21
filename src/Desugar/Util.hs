
module Desugar.Util
	( unflattenApps
	, substituteVV
	, bindingVarsOfStmt
	, bindingVarsOfPat
	, bindingVarsOfGuard
	, takeStmtBoundV
	, bindingVarOfStmt
	, collectClosureProjTags)

where

import Util

import Desugar.Exp
import Type.Exp

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Data.Map	as Map
import Data.Map			(Map)

import Desugar.Plate.Trans


unflattenApps :: a -> [Exp a] -> Exp a
unflattenApps a (x:xs)
 = unflattenApps' a x xs
 
unflattenApps' a x xx
 = case xx of
 	[]	-> x
	xs	
	 -> let	Just xsL	= takeLast xs
	    in	XApp a (unflattenApps' a x (init xs)) xsL


-- | Substitute vars for vars in an expression
substituteVV :: Map Var Var -> Exp a -> Exp a
substituteVV sub xx
 = let	transTable
	 =	(transTableId (\x -> return x))
		{ transV
		    = \v -> case Map.lookup v sub of
				Just v'	-> return v'
				_	-> return v }
   in	evalState (transZM transTable xx) ()
	

-- | Determine the vars being bound by a statement lhs
bindingVarsOfStmt :: Stmt a -> Set Var
bindingVarsOfStmt ss
 = case ss of
	SBind 		_ Nothing x	-> Set.empty
	SBind 		_ (Just v) x	-> Set.singleton v
	SBindMonadic 	_ w x		-> bindingVarsOfPat w
	SBindPat	_ w x		-> bindingVarsOfPat w
	SSig		_ v t		-> Set.singleton v


-- | Determine the vars being bound by a pattern lhs
bindingVarsOfPat :: Pat a -> Set Var
bindingVarsOfPat ww
 = case ww of
	WConLabel 	_ v lvs		-> Set.fromList $ map snd lvs
	WLit		_ lit		-> Set.empty
	WVar		_ v		-> Set.singleton v
	WAt		_ v w		-> Set.unions [Set.singleton v, bindingVarsOfPat w]
	WConLabelP	_ v lws		-> Set.unions $ map bindingVarsOfPat $ map snd lws

-- | Determine the vars being bound by a guard lhs
bindingVarsOfGuard :: Guard a -> Set Var
bindingVarsOfGuard gg
 = case gg of
	GCase 		_ w		-> bindingVarsOfPat w
	GExp		_ w x		-> bindingVarsOfPat w


	
takeStmtBoundV :: Stmt a -> Maybe Var
takeStmtBoundV ss
 = case ss of
 	SBind nn mV x	-> mV
	SSig  nn v t	-> Just v

	
bindingVarOfStmt :: Stmt a -> Maybe Var
bindingVarOfStmt ss
 = case ss of
	SBind sp mv x	-> mv
	_		-> Nothing
				 

-- | Collect all the closure tag vars from the
--	TProjTagged constructors in this expression
collectClosureProjTags :: Exp a -> [Closure]
collectClosureProjTags exp
 = let table	= (transTableId (\x -> return x))
		{ transX_enter = 
			\xx -> case xx of
				XProjTagged n vI tC x j
		 	 	 -> do	modify $ \s -> tC : s
					return xx
				_ -> return xx
		}
   in	execState (transZM table exp) []

