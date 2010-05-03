
module Core.Lift.LiftLambdas
	( liftLambdasP )
where
import Core.Exp
import Core.Glob
import Core.Util
import Core.Plate.Trans
import Core.Plate.FreeVars
import Core.Lift.Base
import Core.Reconstruct
import Type.Util
import Type.Exp
import Type.Builtin
import Util
import DDC.Main.Pretty
import DDC.Var
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import qualified Debug.Trace	as Debug

----
stage	= "Core.Lift.LiftLambdas"
debug	= False
trace ss x	
	= if debug
		then Debug.trace (pprStrPlain ss) x
		else x
		

-- | Lift out all lambda bindings from this super.
--   	return the new super, as well as the lifted bindings as new supers.
--
--   XVars for the supers are left in-place of the lifted bindings.
--	After calling liftLambdasP we still need to go back and rewrite XVars
--	to calls to the new supers (passing in any free variables).
--
liftLambdasP ::	Top	-> LiftM (Top, [Top])
liftLambdasP	p@(PBind superName x)
 = do
	-- Chop out the inner most lambda abstractions from this binding
	--	The new supers get added to the lift state
 	xChopped	<- lambdaLiftX superName Map.empty x
	let pLifted	= PBind superName xChopped

	-- Load the new supers from the stage
	cs		<- getChopped
	let psChopped	= map t3_3 cs

 	return	(pLifted, psChopped)


lambdaLiftX superName vtMore x
	
	-- decend the first set of lambdas in top level bindings
	| XLAM b k x1		<- x
	= do	x1'	<- lambdaLiftX superName (slurpBMore vtMore b) x1
		return	$ XLAM b k x1'	

	| XLam v t e eff clo	<- x
	= do	e'	<- lambdaLiftX superName vtMore e
		return	$ XLam v t e' eff clo
	
	-- walk over the expression and chop out the inner most functions
	| otherwise
	= do	let table	
			= transTableId 
			{ transS 	= chopInnerS superName vtMore
			, decendT	= False }

		transZM table x
		

-- If this binding is a BMore than add the constraint to the map
slurpBMore :: Map Var Type -> Bind -> Map Var Type
slurpBMore vtMore b
 = case b of
 	BMore v t	-> Map.insert v t (vtMore)
	_		-> vtMore

-- | Chop the inner most function from this statement
chopInnerS 
	:: Var		-- the name of the top-level binding that this statement is a part of
			-- used to give chopped functions nicer names

	-> Map Var Type	-- a table of :> constraints on type variables.
			--	we need this when we re-quantify free type vars on lifted supers.

	-> Stmt	
	-> LiftM Stmt

chopInnerS topName vtMore s
 = case s of
 	SBind v x
	 | isFunctionX x
	 , not $ hasEmbeddedLambdasX x
	 -> 	chopInnerS2 topName vtMore s
	 
	_ -> 	return	s

-- | Chop out this function
chopInnerS2 topName vtMore (SBind Nothing x)
 = do	v	<- newVar NameValue
	chopInnerS2 topName vtMore (SBind (Just v) x)

chopInnerS2 topName vtMore (SBind (Just v) x)
 = do	
	-- make a name for the new super
	vN		<- newVar NameValue
	let vSuper	=  vN { varName = (varName topName) ++ "_" ++ (varName vN) }

	-- turn this binding into a super-combinator by binding its free variables as new parameters.
	let pSuper	= PBind vSuper x
	(pSuper_lifted, freeVKs, freeVTs)	
			<- bindFreeVarsP vtMore pSuper

	-- add the new super to the lift state
	addChopped v vSuper pSuper_lifted

	-- Work out the type of the new super.
	let tSuper	= reconP_type (stage ++ ".chopInnerS2") pSuper_lifted

	trace 	( "chopInnerS2: new super\n"
		% " pSuper_lifted:\n" 	%> pSuper_lifted	% "\n\n"
		% " superType:\n" 	%> tSuper		% "\n\n")
		$ return ()
		
	-- build the call to the new super
	let typeArgs	= map makeSuperArgK freeVKs
	let valueArgs	= [XVar v t | (v, t) <- freeVTs]

	let Just xCall	= buildApp (XVar vSuper tSuper : typeArgs ++ valueArgs)

	return	$ SBind (Just v) xCall

	
-- | When we pass args that were free in a lambda abs back to the super, 
--	just pass new witness instead of their args.. we'll thread the actual
--	witness though later on
makeSuperArgK :: (Bind, Kind) -> Exp
makeSuperArgK (b, k)
	| KApp{}	<- k
	, Just t	<- inventWitnessOfClass k
	= XType t
	
	| BVar v	<- b
	= XType (TVar k v)
	
	| BMore v t	<- b
	= XType (TVarMore k v t)


-- | Test whether an expression is syntactically a function.
isFunctionX :: Exp	-> Bool
isFunctionX xx
 = case xx of
	XLAM	v t x	-> isFunctionX x
	XAPP	x t	-> isFunctionX x
	XTau	t x	-> isFunctionX x
	XLam{}		-> True
	_		-> False


--  | Test whether an expression contains any _embedded_ lambda abstractions,
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
	XTau	t x		-> hasEmbeddedLambdasX x
	XLocal	v vs x		-> hasEmbeddedLambdasX x
	XLam	v t x eff clo	-> hasEmbeddedLambdasX x
	
	_			-> hasLambdasX xx
		

-- | Checks whether an expression contains any (value) lambda abstractions.
hasLambdasX ::	Exp 	-> Bool
hasLambdasX	x
 = case x of
	XLAM	v t x		-> hasLambdasX x
	XAPP x t		-> hasLambdasX x
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


-- | Turn this PBind into a combinator by binding its free variables as new parameters.
--   Returns the freshly bound vars and there type/kinds to help construct calls to it
bindFreeVarsP 
	:: Map Var Type			-- table of :> constrains on type variables
	-> Top
	-> LiftM 
		( Top
		, [(Bind, Kind)]	--  type vars that were bound
		, [(Var,  Type)])	-- value vars that were bound
		

bindFreeVarsP
	vtMore
	(PBind vTop x)
 = do
	-- Work out the non-top-level vars that are free in the expresison.
	cgHeader	<- gets stateHeaderGlob
	cgModule	<- gets stateModuleGlob 

	let vsFree_local	
		= filter (\v -> (not $ varIsBoundAtTopLevelInGlob cgModule v)
			     && (not $ varIsBoundAtTopLevelInGlob cgHeader v))
		$ filter (\v -> elem (varNameSpace v) [NameValue, NameType])
		$ Set.toList 
		$ freeVars x	

	let (vsFreeVal, vsFreeType)
		= partition (\v	-> varNameSpace v == NameValue)
		$ vsFree_local

 	-- Work out the type of the expression
	--	The effect should always be TBot because we only ever lift (value) lambda abstractions.
	--	The closure returned from reconX will be flat, ie a TSum of all the free vars
	let (xRecon, tRecon, _, cRecon) 
			= reconX' (stage ++ ".bindFreeVarsP") x

	-- Bind free value vars with new value lambdas.
	-- TODO: better to get the type directly from the XVar
	tsFreeVal	<- mapM getType vsFreeVal
	let vtsFreeVal	= zip vsFreeVal tsFreeVal
	let (xBindVal, _)
			= foldr addLambda (xRecon, cRecon) vtsFreeVal

	-- Bind free type vars with new type lambdas
	-- TODO: better to get the kind directly from the TVar
	ksFreeType	<- mapM getKind vsFreeType
	let bksFreeType	= zipWith
				(\v k -> case Map.lookup v vtMore of
						Nothing		-> (BVar v, k)
						Just tMore	-> (BMore v tMore, k))
				vsFreeType
				ksFreeType

	let xBindType	= addLAMBDAs bksFreeType xBindVal

	-- make the top level binding
	let pBound	= PBind vTop xBindType

	-- add the new type to the liftM state straight away, so if we had nested lambda bindings,
	--	the next time we add lambdas we'll know what the type of this one was.
	bindType vTop (reconP_type (stage ++ ".bindFreeVarsP") pBound)

	trace
	 (pprStrPlain	$ "\n\n\n"
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
 = let	-- filter out the closure term corresponding to the var that is bound
	--	by this lambda.
	clo'	= makeTSum kClosure
		$ filter (\c -> case c of
					TFree v' _	-> v /= v
					_		-> True)
		$ flattenTSum clo 

   in	(XLam v t x tPure clo', clo')


