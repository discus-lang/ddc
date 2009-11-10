
module Core.Lift.BindTypes
	( bindTypesTree )

where

import Util

import Core.Exp
import Core.Lift.Base
import Core.Plate.Trans
import Core.Reconstruct
import Core.Util
import Core.Util.Slurp


bindTypesTree :: Tree 	-> LiftM ()
bindTypesTree	 ps
 = do
 	let table
		= transTableId
		{ transP	= bindTypesP
		, transS	= bindTypesS
		, transX	= bindTypesX
		, transW	= bindTypesW }

	transZM table ps
	
	return ()


-----
bindTypesP ::	Top	-> LiftM Top
bindTypesP	p
 = case p of
 	PExtern v tv to	
	 -> do	bindType  v tv
		return p

	PBind v (XAnnot aa x)
	 -> do 	let [t]	= [t | NType t <- aa]
	 
	 	bindType  v t
		return p
	
	_ -> return p


bindTypesS ::	Stmt	-> LiftM Stmt
bindTypesS	s
 = case s of
	SBind (Just v) x
	 -> do	let t	= reconX_type "Core.Lift.BindTypes" x
	 	-- let Just t	= maybeSlurpTypeX x
	 	bindType v t
	 	return s

	_ -> return s


bindTypesX ::	Exp	-> LiftM Exp
bindTypesX	x
 = case x of
 	XLam v t e eff clo
	 -> do	bindType v t
		return x

	_ -> return x


bindTypesW ::	Pat	-> LiftM Pat
bindTypesW	ww
 = case ww of
 	WCon _ v lvts
	 -> do	mapM (\(l, v, t) -> bindType v t) lvts
	 	return ww
		
	_	-> return ww
	



