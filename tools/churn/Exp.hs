module Exp 
	( genExp, genExpT
	, genAltTT 
	, genGuardT
	, genType
	, genVar)
where
	
import Bits
import Base

import Source.Exp

import Type.Exp

import Shared.Var	(NameSpace(..))
import Shared.VarPrim
import qualified Shared.Var	as Var

import Util

import System.Random


-- generate a random expression, starting from an empty environment
genExp :: GenM (Exp a)
genExp 
 = do	t	<- genType True
 	x	<- genExpT initEnv t
	return	$ x

-- Exp ---------------------------------------------------------------------------------------------
-- generate a random expression
genExpT 
	:: Env			-- environment
	-> Type			-- type of expression to generate
	-> GenM (Exp a)

genExpT env tt
 = do	r_	<- genInt 0 5
 	fuel	<- checkFuel
	
	let r	| fuel > 0	= r_
		| otherwise	= 0
	
	let result
		| r <= 1
		= genExpT_base env tt

		| r <= 2
		= genExpT_var env tt
		
		| r <= 4
		= genExpT_app  env tt
		
		| r <= 5
		= genExpT_match env tt
	result
		

-- Exp Base ----------------------------------------------------------------------------------------
-- | Make an expression as a base value.

genExpT_base env tt
	-- unit
	| TData KValue v []			<- tt
	, v == primTUnit
	= do	burn 1
		return $ XVar none (primUnit)

	-- literal
	| TData (KFun KRegion KValue) v [TWild KRegion]	<- tt
	, v == primTInt 
	= do	burn 1
		r	<- genInt 0 100
		return	$ XConst none (CConst (LInt r))
		

	-- function
	| TFun t1 t2 _ _		<- tt
	= do	burn 1
		v	<- genVar NameValue
		xBody	<- genExpT [(t1, v)] t2 
		return	$  XLambdaPats none [WVar none v] xBody


-- Exp Var -----------------------------------------------------------------------------------------
-- | Make an expression by referencing something 

genExpT_var env tt
 = do
 	-- try and find something from the environment of the desired type
	let  candidates	= filter (\(t, v) -> t == tt) env

	ix 	<- genInt 0 (length candidates -1)

	let result
		-- no candidates, try again
		| []		<- candidates
		= genExpT env tt
		
		| (tVar, var)	<- candidates !! ix
		= return	$ XVar none var
 	
	result
	

-- Exp App -----------------------------------------------------------------------------------------
-- | Make an expression of this type, and do it by applying a function in the environment

genExpT_app env tt
 = do	
 	-- try and find a function in the environment that can give us
	--	the result type we're after
	let  candidates	= filter (\(t, v) -> resultTypeT t == tt
					  && isFun t)
			$ env
			
	ix 	<- genInt 0 (length candidates -1)
			
	let result
		-- no candidates of this type, try again
		| []		<- candidates
		= genExpT env tt
		
		| (tFun, vFun)	<- candidates !! ix
		= genExpT_call env tt tFun vFun
	
	result
	

genExpT_call env tt tFun vFun
 = do	
 	-- make the arguments to the call
 	let tsArgs	= argTypesT tFun
	burn (length tsArgs)
 	xsArgs		<- mapM (genExpT env) tsArgs
	return	$ makeCall (XVar none vFun : xsArgs)


-- Exp Match ---------------------------------------------------------------------------------------
-- | Make an expression of this type, and do it via a match expression.

genExpT_match env tRHS
 = do	burn 1
	-- choose the number of alternatives
 	nAlts	<- genInt 1 5
	
	-- choose a type for the lhs
	tLHS	<- genType False 
	
	-- generate the alternatives
	alts	<- replicateM nAlts (genAltTT env tLHS tRHS)
	
	return	$ XMatch none alts



-- Alternative -------------------------------------------------------------------------------------
genAltTT :: Env -> Type -> Type -> GenM (Alt a)
genAltTT env tLHS tRHS
 = do	burn 1

	-- choose number of guards
	nGuards	<- genInt 1 3

 	guards	<- replicateM nGuards (genGuardT env tLHS)
	xRHS	<- genExpT env tRHS
	
	return	$ AAlt none guards xRHS		


-- Guard -------------------------------------------------------------------------------------------
genGuardT :: Env -> Type -> GenM (Guard a)
genGuardT env tLHS
 = do	burn 1
 	
	tGuard	<- genType False 
	xExp	<- genExpT env tGuard
	wPat	<- genPatT tGuard
	
	return	$ GExp none wPat xExp

-- Pat ---------------------------------------------------------------------------------------------
genPatT :: Type -> GenM (Pat a)
genPatT tt
 	| TData KValue v []	<- tt
	, v == primTUnit
	= chooseM 
		[ do	return (WUnit none)

		, do	var	<- genVar NameValue
			return	$ WVar none var]
	
	| TData (KFun KRegion KValue) v _	<- tt
	, v == primTInt
	= chooseM
		[ do	var	<- genVar NameValue
			return	$ WVar none var]
			
	


-- Exp Constant ------------------------------------------------------------------------------------
genVar :: NameSpace -> GenM Var
genVar space
 = do	ix	<- gets stateVarGen
 	modify $ \s -> s { stateVarGen = ix + 1 }
	
	let var	= (Var.new ("v" ++ show ix))
			{ Var.nameSpace = space }
	
	return	var


-- Type --------------------------------------------------------------------------------------------
-- generate a random type
genType :: Bool -> GenM Type
genType genFun
 = do	
 	-- only generate functional types if we were asked for them.
 	r_	<- if genFun
 			then genInt 0 2
			else genInt 0 1

	fuel	<- checkFuel
	
	let r	| fuel > 0	= r_
		| otherwise	= 0

	let result
		| r == 0	
		= do	burn 1
			return $ TData KValue primTUnit []

		| r <= 1
		= do	burn 1
			return $ TData (KFun KRegion KValue) primTInt  [TWild KRegion]

		| r <= 2
		= do	burn 1
		 	t1	<- genType genFun
			t2	<- genType genFun
			return	$ TFun t1 t2 (TWild KEffect) (TWild KClosure)	
		
			
	result
