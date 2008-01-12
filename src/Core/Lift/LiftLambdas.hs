
module Core.Lift.LiftLambdas
	( liftLambdasP )

where

import Util

import qualified Shared.VarUtil	as Var
import qualified Shared.Var	as Var
import Shared.Var		(Var, NameSpace(..))

import qualified Data.Set	as Set
import Data.Set			(Set)

import Core.Exp
import Core.Util
import Core.Util.Slurp
import Core.Plate.Trans
import Core.Plate.FreeVars
import Core.Lift.Base
import Core.Reconstruct

import qualified Debug.Trace	as Debug

debug	= False
trace ss x	
	= if debug
		then Debug.trace (pretty ss) x
		else x
		

-----
stage	= "Core.Lift.LiftLambdas"

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
	--	The new supers get added to the lift state
 	xChopped	<- lambdaLiftX superName x
	let pLifted	= PBind superName xChopped

	-- Load the new supers from the stage
	cs		<- getChopped
	let psChopped	= map t3_3 cs

 	return	(pLifted, psChopped)


lambdaLiftX	superName x
	
	-- decend the first set of lambdas in top level bindings
	| XLAM v k e		<- x
	= do	e'	<- lambdaLiftX superName e
		return	$ XLAM v k e'	

	| XLam v t e eff clo	<- x
	= do	e'	<- lambdaLiftX superName e
		return	$ XLam v t e' eff clo
	
	-- walk over the expression and chop out the inner most functions
	| otherwise
	= do
		let table	
			= transTableId 
			{ transS 	= chopInnerS superName
			, decendT	= False }

		transZM table x
		

-- | Chop the inner most function from this statement
--	
chopInnerS 
	:: Var		-- the name of the top-level binding that this statement is a part of
			-- used to give chopped functions nicer names
	-> Stmt	
	-> LiftM Stmt

chopInnerS topName s
 = case s of
 	SBind v x
	 | isFunctionX x
	 , not $ hasEmbeddedLambdasX x
	 -> 	chopInnerS2 topName s
	 
	_ -> 	return	s

-- | Chop out this function
chopInnerS2	topName	(SBind Nothing x)
 = do	v		<- newVar Var.NameValue
	chopInnerS2 topName (SBind (Just v) x)

chopInnerS2	topName	(SBind (Just v) x)
 = do	
	-- make a name for the new super
	vN		<- newVar Var.NameValue
	let vSuper	=  vN { Var.name = (Var.name topName) ++ "_" ++ (Var.name vN) }
	
	-- remember that a new binding with this name is to be created at top level
	modify $ \s -> s { stateTopVars	= Set.insert vSuper $ stateTopVars s }

	-- turn this binding into a super-combinator by binding its free variables as new parameters.
	let pSuper	= PBind vSuper x
	(pSuper_lifted, freeVKs, freeVTs)	
			<- bindFreeVarsP pSuper

	-- add the new super to the lift state
	addChopped v vSuper pSuper_lifted

	-- Work out the type of the new super.
	let tSuper	= reconP_type (stage ++ ".chopInnerS2") pSuper_lifted
	
	-- build the call to the new super
	let typeArgs	= map makeSuperArgK freeVKs
	let valueArgs	= [XVar v t | (v, t) <- freeVTs]
	let callArgs	= typeArgs ++ valueArgs

	let Just xCall	= buildApp (XVar vSuper tSuper : typeArgs ++ valueArgs)

	return	$ SBind (Just v) xCall

	
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
	_		-> XType (TVar k (varOfBind b))
	




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
	
hasLambdasA	a
 = case a of
 	AAlt 	gs x	-> hasLambdasX x

hasLambdasG	gg
 = case gg of
	GExp p x	-> hasLambdasX x 	






-- | Turn this PBind into a combinator by binding its free variables as new parameters.
--   Returns the freshly bound vars and there type/kinds to help construct calls to it
--
bindFreeVarsP 
	:: Top
	-> LiftM 
		( Top
		, [(Bind, Kind)]	--  type vars that were bound
		, [(Var,  Type)])	-- value vars that were bound
		

bindFreeVarsP
	(PBind vTop x)
 = do

	-- Work out the vars which are free in the expression
	vsBoundTop	<- gets stateTopVars
 	let vsFree	= Set.filter (\v -> not $ Var.isCtorName v)
			$ Set.difference (freeVars x) vsBoundTop

	let (vsFreeVal, vsFreeType)
			= partition (\v	-> Var.nameSpace v == NameValue)
			$ Set.toList vsFree

 	-- Work out the type of the expression
	--	The effect should always be TBot because we only ever lift (value) lambda abstractions.
	--	The closure returned from reconX will be flat, ie a TSum of all the free vars
	let (xRecon, tRecon, TBot KEffect, cRecon) 
			= reconX' (stage ++ ".bindFreeVarsP") x

	-- Take the closure annotation from the inner most lambda. 
	--	We'll use this instead of cRecon when adding more closure annotations because it's more
	--	likely to be a variable (ie not a TSum) so the output will be more readable.
	-- 
	-- TODO: nice idea, but the XTet binding it is in the wrong place.
--	let Just cTake	= takeXLamClosure xRecon

	-- Bind free value vars with new value lambdas.
	-- TODO: better to get the type directly from the XVar
	tsFreeVal	<- mapM getType vsFreeVal
	let vtsFreeVal	= zip vsFreeVal tsFreeVal
	let (xBindVal, cBindVal)
			= foldr addLambda (xRecon, cRecon) vtsFreeVal

	-- Bind free type vars with new type lambdas
	-- TODO: better to get the type directly from the TVar
	-- BUGS: bound quantification isn't being preserved here

	ksFreeType	<- mapM getKind vsFreeType
	let bksFreeType	= zip (map BVar vsFreeType) $ ksFreeType
	let xBindType	= addLAMBDAs bksFreeType xBindVal

	-- make the top level binding
	let pBound	= PBind vTop xBindType

	-- add the new type to the liftM state straight away, so if we had nested lambda bindings,
	--	the next time we add lambdas we'll know what the type of this one was.
	bindType vTop (reconP_type (stage ++ ".bindFreeVarsP") pBound)

	trace
	 (pretty	$ "\n\n\n"
	 		% "* bindFreeVarsP\n"
			% "    vTop      = "	% vTop		% "\n"
			% "    x:\n"		%> x		% "\n\n"
	 		% "    tRecon:\n"	%> tRecon	% "\n\n"
			% "    cRecon:\n"	%> cRecon	% "\n\n"
			% "    xBindVal:\n"	%> xBindVal	% "\n\n")

	 $ return 
	 	( pBound
	 	, bksFreeType
		, vtsFreeVal)


-- | Add a value lambda to the outside of this expression.
--	the effect  is set to TBot

addLambda :: (Var, Type) -> (Exp, Closure) -> (Exp, Closure)
addLambda (v, t) (x, clo)
 = let	clo'	= makeTMask KClosure clo (TTag v)
   in	(XLam v t x (TBot KEffect) clo', clo')


-- | Take the closure annotation from the outermost XLam in this expression
takeXLamClosure :: Exp -> Maybe Closure
takeXLamClosure xx
 = case xx of
	XAnnot 	nn x		-> takeXLamClosure x
	XLAM	v t x		-> takeXLamClosure x
	XTet	vts x		-> takeXLamClosure x
	XTau	t x		-> takeXLamClosure x
	XLam	v k x eff clo	-> Just clo
	_			-> Nothing
