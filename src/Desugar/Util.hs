
module Desugar.Util
	( unflattenApps
	, takeStmtBoundV
	, bindingVarOfStmt
	, substituteVV)
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
	

substituteVV :: Map Var Var -> Exp a -> Exp a
substituteVV sub xx
 = let	transTable
	 =	(transTableId (\x -> return x))
		{ transV
		    = \v -> case Map.lookup v sub of
				Just v'	-> return v'
				_	-> return v }
   in	evalState (transZM transTable xx) ()
	
				 










