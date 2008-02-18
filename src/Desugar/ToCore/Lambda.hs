
module Desugar.ToCore.Lambda
	( fillLambdas
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
import Core.Plate.FreeVars

import Desugar.ToCore.Base

import qualified Type.ToCore	as T
import qualified Type.Exp	as T
import qualified Type.Util	as T

-----
stage 		= "Desugar.ToCore.Lambda"
debug		= True
trace ss x	= if debug 
			then Debug.trace (pprStr ss) x
			else x

-- | Add type lambdas and contexts to this expression, based on the provided type scheme.
--	Used on RHS of let-bindings.
fillLambdas
	:: Var		-- name of bound variable
	-> Type		-- type scheme
	-> Exp		-- rhs expression
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

	| otherwise
	= return x


-- | Create an effect annotation to attach to an XLam
loadEffAnnot 
	:: Effect 	-- the tag var from the desugared code (or TBot)
	-> CoreM Effect	

loadEffAnnot ee
 = case ee of
	TVar KEffect vE	
	 -> do	Just tE		<- lookupType vE
{-		trace	( "*   loadEffAnnot\n"
			% "    vE       = " % vE	% "\n"
			% "    tE       = " % tE	% "\n"
			% "    tE_bound = " % tE_bound	% "\n"
			% "    free = " % freeVars tE	% "\n")
		-}
		return	$ flattenT $ stripContextT tE

 	TBot KEffect	
	  -> 	return	$ TBot KEffect


-- Load a closure annotation to attach to an XLam
loadCloAnnot 
	:: Closure 	-- the tag var from the desugared code (or TBot)
	-> CoreM Closure

loadCloAnnot cc
 = case cc of
	TVar KClosure vC	
	 -> do	Just tC		<- lookupType vC
	 	return 	$ trimClosureC Set.empty Set.empty 
			$ flattenT $ stripContextT tC
			 
	TBot KClosure 	
	 -> 	return	$ TBot KClosure


{-
attachBounds :: Type -> CoreM Type
attachBounds tt
 = do	quantVars	<- gets coreQuantVars
	let fsBound	= [ FMore v (T.toCoreT t)
				| v		<- Set.toList $ freeVars tt
				, (k, Just t)	<- Map.lookup v quantVars ]
				
	return	$ makeTFetters tt fsBound	
-}
