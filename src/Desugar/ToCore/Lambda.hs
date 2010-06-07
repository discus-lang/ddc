
module Desugar.ToCore.Lambda
	( fillLambdas
	, loadEffAnnot 
	, loadCloAnnot)
where
import Core.Exp
import Core.Util
import Desugar.ToCore.Base
import DDC.Type
import DDC.Var
import qualified Data.Set 	as Set
import qualified Data.Map	as Map


-- | Add type lambdas and contexts to this expression, based on the provided type scheme.
--	Used on RHS of let-bindings.
fillLambdas
	:: Var		-- name of bound variable
	-> Type		-- type scheme
	-> Exp		-- rhs expression
	-> CoreM Exp
	
fillLambdas v tScheme x
 =	fillLambdas' Map.empty tScheme x
	
fillLambdas' tsWhere tScheme x
	-- Give this witness a new name so we can refer to it later on.
	| TForall BNil k tRest		<- tScheme
	= do	x'	<- fillLambdas' tsWhere tRest x
		v'	<- newVarN NameClass
		return	$ XLAM (BVar v') k x'

 	| TForall b k tRest		<- tScheme
	= do	x'	<- fillLambdas' tsWhere tRest x
		return	$ XLAM b k x'

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
	TVar kE (UVar vE)
	 | kE == kEffect
	 -> do	Just tE		<- lookupType vE
		return	$ flattenT $ stripContextT tE

 	TSum kE []
	 | kE == kEffect
	 -> 	return	$ tPure


-- Load a closure annotation to attach to an XLam
loadCloAnnot 
	:: Closure 	-- the tag var from the desugared code (or TBot)
	-> CoreM Closure

loadCloAnnot cc
 = case cc of
	TVar kC (UVar vC)
	 | kC == kClosure
	 -> do	Just tC		<- lookupType vC
	 	return 	$ trimClosureC_constrainForm Set.empty Set.empty 
			$ flattenT_constrainForm 
			$ stripContextT tC
			 
	TSum kC []
	 | kC == kClosure 	
	 -> 	return	$ tEmpty


