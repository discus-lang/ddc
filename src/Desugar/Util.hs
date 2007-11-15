
module Desugar.Util
	( unflattenApps
	, takeStmtBoundV
	, takeAnnotX 
	, collectVars 
	, bindingVarOfStmt )
where

import Util

import Desugar.Exp
import Type.Exp

import qualified Data.Set	as Set
import Data.Set			(Set)

import Desugar.Plate.Trans

-----
unflattenApps :: a -> [Exp a] -> Exp a
unflattenApps a (x:xs)
 = unflattenApps' a x xs
 
unflattenApps' a x xx
 = case xx of
 	[]	-> x
	xs	
	 -> let	Just xsL	= takeLast xs
	    in	XApp a (unflattenApps' a x (init xs)) xsL

-----
takeStmtBoundV :: Stmt a -> Maybe Var
takeStmtBoundV ss
 = case ss of
 	SBind nn mV x	-> mV
	SSig  nn v t	-> Just v

-----
takeAnnotX :: Exp a -> Maybe a
takeAnnotX xx
 = case xx of
 	XNil					-> Nothing
	XVoid		a			-> Just a
	XConst		a c			-> Just a
	XVar		a v			-> Just a
	XProj 		a x j			-> Just a
	XLambda		a v x			-> Just a
	XApp		a x1 x2			-> Just a
	XMatch 		a x aa			-> Just a
	XDo		a ss			-> Just a
	XIfThenElse 	a x1 x2 x3		-> Just a

	----- 
	XLambdaTEC  	a v x1 t eff clo	-> Just a
	XVarInst	a v			-> Just a
	XProjTagged	a v x j			-> Just a


-----
collectVars xx
 = let	transTable 	
 	  = 	(transTableId (\x -> return x))
		{ transV	= \v ->
		  do	modify (Set.insert v)
			return v }
	
   in	execState (transZM transTable xx) Set.empty
	
	
bindingVarOfStmt :: Stmt a -> Maybe Var
bindingVarOfStmt ss
 = case ss of
	SBind sp mv x	-> mv
	_		-> Nothing
	











