
module Desugar.ToCore.Util
	( pushLambdaXDo
	, doMe 
	, dropXTau )

where

import Shared.Var (Var)
import qualified Core.Exp 	as C
import qualified Core.Util	as C

import Desugar.ToCore.Base

-----
stage	= "Desugar.ToCore.Util"


-----------------------
-- pushLambdaXDo
--	Makes sure that the body of an inner most lambda is an XDo
--
pushLambdaXDo :: C.Exp -> C.Exp
pushLambdaXDo	 xx
 = case xx of
	C.XLAM{}	-> pushLambdaXDo' xx
 	C.XLam{}	-> pushLambdaXDo' xx
	_		-> xx
	
pushLambdaXDo'	xx
 = case xx of
	C.XLAM v k e
	 -> C.XLAM v k $ pushLambdaXDo' e

 	C.XLam v t e eff clo	
	 -> C.XLam v t (pushLambdaXDo' e) eff clo
	
	C.XDo [C.SBind _ x]
	 -> pushLambdaXDo' x
	 		
	C.XDo{}
	 -> xx

	C.XAnnot _ (x@C.XDo  {})	-> pushLambdaXDo' x
	C.XAnnot _ (x@C.XLam {})	-> pushLambdaXDo' x

--	C.XAnnot [C.NType t] x 	
--	 -> C.XDo [C.SBind Nothing t x]

	_ -> C.XDo [C.SBind Nothing xx]
 

--	_ -> panic stage
--		$ "pushLambdaXDo: no match for " % show xx % "\n"
	
doMe :: 	C.Exp -> C.Exp
doMe		x
 = case x of
	C.XLam v t x' eff clo	-> C.XLam v t (doMe x') eff clo
	C.XLAM v k x'		-> C.XLAM v k (doMe x')
 	C.XDo{}			-> x
	_			-> C.XDo [C.SBind Nothing x]


stripAnnot xx
 = case xx of
 	C.XAnnot _ x	-> x
	_		-> xx


-- | Decend into this expression and annotate the first value found with its type
--	doing this makes it possible to slurpType this expression
--
dropXTau :: C.Exp -> [(Var, C.Type)] -> C.Type -> C.Exp
dropXTau xx env tt
	-- decend into XLAMs
	| C.XLAM v t x		<- xx
	, C.TForall v t1 t2	<- tt
	= C.XLAM v t $ dropXTau x env t2
	
	| C.XLAM v t x		<- xx
	, C.TContext t1 t2	<- tt
	= C.XLAM v t $ dropXTau x env t2
	
	-- decend into XLams
	| C.XLam v t x eff clo	<- xx
	, C.TFunEC _ t2 _ _	<- tt
	= C.XLam v t (dropXTau x env t2) eff clo
	
	-- skip over XTets
	| C.XTet vts x		<- xx
	= C.XTet vts $ dropXTau x env tt
	
	-- load up bindings into the environment
	| C.TWhere t vts	<- tt
	= dropXTau xx (vts ++ env) t

	-- there's already an XTau here,
	--	no point adding another one, 
	--	bail out
	| C.XTau t x		<- xx
	= xx
	
	-- we've hit a value, drop the annot
	| otherwise
	= C.XTau (C.packT $ C.makeTWhere tt env) xx
	







