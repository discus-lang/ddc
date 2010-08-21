{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Desugar.ToCore.Lambda
	( fillLambdas
	, loadEffAnnot 
	, loadCloAnnot)
where
import DDC.Main.Error
import DDC.Desugar.ToCore.Base
import DDC.Core.Exp
import DDC.Type
import DDC.Var
import qualified Data.Map	as Map

stage	= "DDC.Desugar.ToCore.Lambda"

-- | Add type lambdas and contexts to this expression, based on the provided type scheme.
--	Used on RHS of let-bindings.
fillLambdas
	:: Var		-- name of bound variable
	-> Type		-- type scheme
	-> Exp		-- rhs expression
	-> CoreM Exp
	
fillLambdas _ tScheme x
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

	| TConstrain tRest crs		<- tScheme
	= do	let tsWhere'	= Map.union tsWhere (crsEq crs)
		x'	<- fillLambdas' tsWhere' tRest x
		return	x'
	
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
		return	$ flattenT $ stripToBodyT tE

 	TSum kE []
	 | kE == kEffect
	 -> 	return tPure

	_ 	-> panic stage
		$  "loadEffAnnot: no match"


-- Load a closure annotation to attach to an XLam
loadCloAnnot 
	:: Closure 	-- the tag var from the desugared code (or TBot)
	-> CoreM Closure

loadCloAnnot cc
 = case cc of
	TVar kC (UVar vC)
	 | kC == kClosure
	 -> do	Just tC		<- lookupType vC
	 	return 	$ trimClosureC $ flattenT $ stripToBodyT tC
			 
	TSum kC []
	 | kC == kClosure 	
	 -> 	return	$ tEmpty

	_ 	-> panic stage
		$  "loadEffAnnot: no match"


