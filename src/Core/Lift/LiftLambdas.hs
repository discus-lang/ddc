
module Core.Lift.LiftLambdas
	( liftLambdasP )

where

import Util

import qualified Shared.Var	as Var
import Shared.Var		(Var, NameSpace(..))

import qualified Data.Set	as Set
import Data.Set			(Set)

import Core.Exp
import Core.Util
import Core.Util.Slurp
import Core.Plate.Trans
import Core.Lift.Base
import Core.Lift.LambdaFree

-----------------------
-- liftLambdasP
--	Lift out all lambda bindings from this super.
--	return the new super, as well as the lifted bindings as new supers.
--
--	XVars for the supers are left in-place of the lifted bindings.
--	After calling liftLambdasP we still need to go back and rewrite XVars
--	to calls to the new supers (passing in any free variables).
--
liftLambdasP ::	Top	-> LiftM (Top, [Top])
liftLambdasP	p@(PBind superName x)
 = do
	-- Chop out the inner most lambda abstractions from this binding
	--
 	xChopped	<- lambdaLiftX superName x
	let pLifted	= PBind superName xChopped

	cs		<- getChopped
	let psChopped	= map t3_3 cs

 	return	(pLifted, psChopped)


lambdaLiftX	superName x
	
	-- decend the first set of lambdas
	--	in top level bindings
	| XLAM v k e		<- x
	= do	e'	<- lambdaLiftX superName e
		return	$ XLAM v k e'	

	| XLam v t e eff clo	<- x
	= do	e'	<- lambdaLiftX superName e
		return	$ XLam v t e' eff clo
	
	-- chop out lambda bindings
	| otherwise
	= do
		let table	
			= transTableId 
			{ transS 	= chopInnerS superName
			, decendT	= False }

		transZM table x
		

-----
chopInnerS ::	Var	-> Stmt	-> LiftM Stmt
chopInnerS	topName	   s
 = case s of
 	SBind v x
	 | isFunctionX x
	 , not $ hasEmbeddedLambdasX x
	 -> 	chopInnerS2 topName s
	 
	_ -> 	return	s


chopInnerS2	topName	(SBind Nothing x)
 = do	v		<- newVar Var.NameValue
	chopInnerS2 topName (SBind (Just v) x)

chopInnerS2	topName	(SBind (Just v) x)
 = do	
	-- make a name for the new super
	vN		<- newVar Var.NameValue
	let vSuper	=  vN { Var.name = (Var.name topName) ++ "_" ++ (Var.name vN) }
	
	-- add the new super to the Lift state
	let pSuper			= PBind vSuper x

	modify (\s -> s { stateTopVars	= Set.insert vSuper $ stateTopVars s })

	(pSuperL, freeVKs, freeVTs)	<- lambdaFreeVarsP pSuper

	addChopped v vSuper pSuperL

	-- replace this binding with the name of the super
	let Just t	= maybeSlurpTypeX x
	let x'		= XTau t 
			$ unflattenApps
			$ (XVar vSuper TNil
				: (  map makeSuperArgK freeVKs
				  ++ [XVar v t | (v, t) <- freeVTs]))
			
			
	return	$ SBind (Just v) x'
	
-----
-- makeSuperArg
--	When we pass args that were free in a lambda abs back to the super, 
--	just pass new witness instead of their args.. we'll thread the actual
--	witness though later on
--
makeSuperArgK :: (Bind, Kind) -> Exp
makeSuperArgK (b, k)
 = case k of
 	KClass v ts	-> XType (TClass v ts)
	_		-> XVar (varOfBind b) TNil
	




-- | Tests whether an expression is syntactically a function.
isFunctionX :: Exp	-> Bool
isFunctionX xx
 = case xx of
	XAnnot 	nn x	-> isFunctionX x
	XLAM	v t x	-> isFunctionX x
	XAPP	x t	-> isFunctionX x
	XTet	vts x	-> isFunctionX x
	XTau	t x	-> isFunctionX x
	XLam{}		-> True
	_		-> False


-- hasEmbeddedLambdas
-- 	| Tests whether an expression contains any _embedded_ lambda abstractions,
--		not including the outermost set.
--
--	eg \x -> \y -> f x 3		-- no embedded lambdas
--	eg \x -> \y -> f (\z -> z) 3	-- embeded lambda in first arg
--
hasEmbeddedLambdasX ::	Exp -> Bool
hasEmbeddedLambdasX xx
 = case xx of
 	XLAM	v t x		-> hasEmbeddedLambdasX x
	XAPP 	x t		-> hasEmbeddedLambdasX x
	XTet	vts x		-> hasEmbeddedLambdasX x
	XTau	t x		-> hasEmbeddedLambdasX x
	XLocal	v vs x		-> hasEmbeddedLambdasX x
	XLam	v t x eff clo	-> hasEmbeddedLambdasX x
	
	_			-> hasLambdasX xx
		

-----------------------
-- hasLambdasX
--	Checks whether an expression contains any (value) lambda abstractions.
--
hasLambdasX ::	Exp 	-> Bool
hasLambdasX	x
 = case x of
	XAnnot 	a x		-> hasLambdasX x

	XLAM	v t x		-> hasLambdasX x
	XAPP x t		-> hasLambdasX x
	XTet	vts x		-> hasLambdasX x
	XTau	t x		-> hasLambdasX x

	XLam{}			-> True
	XApp x1 x2 eff		-> hasLambdasX x1 || hasLambdasX x2
	XDo ss			-> or $ map hasLambdasS ss
	XMatch aa		-> or $ map hasLambdasA aa
	
	XLocal v vs x		-> hasLambdasX x

	_			-> False
	
hasLambdasS	s
 = case s of
	SBind v x	-> hasLambdasX x
	_		-> False
	
hasLambdasA	a
 = case a of
 	AAlt 	gs x	-> hasLambdasX x

hasLambdasG	gg
 = case gg of
	GExp p x	-> hasLambdasX x 	









