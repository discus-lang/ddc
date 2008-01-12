
module Desugar.ToCore.Lambda
	( fillLambdas
	, addTau 
	, loadEffAnnot 
	, loadCloAnnot)

where

import Util
import qualified Debug.Trace	as Debug

import qualified Data.Set 	as Set
import Data.Set			(Set)

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Shared.Var 	as Var
import Shared.Var		(VarBind, NameSpace(..))
import Shared.Error

import Core.Exp
import Core.Util

import Desugar.ToCore.Base

import qualified Type.ToCore	as T
import qualified Type.Exp	as T

-----
stage 		= "Desugar.ToCore.Lambda"
debug		= True
trace ss x	= if debug 
			then Debug.trace (pretty ss) x
			else x

-- | Add a LAMBDAs around this expression for each forall / context in the provided type.
fillLambdas
	:: Var
	-> Type
	-> Exp
	-> CoreM Exp
	
fillLambdas v tScheme x
 = {-trace 
 	("* Desugar.ToCore.fillLambdas\n"
 	% "    v       = " % v % 	"\n"
	% "    tScheme =\n" %> tScheme %	"\n") -}
	(fillLambdas' Map.empty tScheme x)	
	
	
fillLambdas' tsWhere tScheme x
 	| TForall v k tRest		<- tScheme
	= do	x'	<- fillLambdas' tsWhere tRest x
		return	$ XLAM v k x'
	
	-- Give this witness a new name so we can refer to it later on.
	| TContext c tRest		<- tScheme
	= do	x'	<- fillLambdas' tsWhere tRest x
		v'	<- newVarN NameClass
		return	$ XLAM (BVar v') c x'

	| TFetters tRest fs		<- tScheme
	= do	let tsWhere'	= Map.union tsWhere (Map.fromList [(v, t) | FWhere v t <- fs])
		x'	<- fillLambdas' tsWhere' tRest x
		return	$ x'

{-	| TFunEC t1 t2 eff clo		<- tScheme
	, XLam v _ x' eff clo		<- x
 	= do	x2		<- fillLambdas' tsWhere t2 x'
--		effAnnot	<- loadEffAnnot eff
--		cloAnnot	<- loadCloAnnot clo
		return	$ XLam v t1 x2 eff clo -- effAnnot cloAnnot
-}
	| otherwise
	= return x


loadEffAnnot :: Effect -> CoreM Effect
loadEffAnnot ee
 = case ee of
	TVar KEffect vE	
	 -> do	Just vT	<- lookupType vE
	 	return	$ packT $ flattenT $ stripContextT vT

 	TBot KEffect	
	  -> 	return	$ TBot KEffect


loadCloAnnot :: Closure -> CoreM Closure
loadCloAnnot cc
 = case cc of
	TVar KClosure vC	
	 -> do	Just vT	<- lookupType vC
	 	return	$ packT $ trimClosureC $ flattenT $ stripContextT vT
			 
	TBot KClosure 	
	 -> 	return	$ TBot KClosure


addTau xT x
	| gotAnnot x	= x
	| otherwise	= XTau xT x
	
gotAnnot x
 = case x of
 	XLAM{}		-> True
	XLam{}		-> True
	XTau{}		-> True
	XTet vts x	-> gotAnnot x
	_		-> False




