
module Core.Lift.LiftLambdas
	( liftLambdasP )
where
import Core.Util
import Core.Plate.Trans
import Core.Plate.FreeVars
import Core.Lift.Base
import Core.Reconstruct
import Util
import DDC.Main.Pretty
import DDC.Core.Exp
import DDC.Core.Glob
import DDC.Core.Lint.Exp
import DDC.Core.Lint.Env
import DDC.Type
import DDC.Var
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import qualified Debug.Trace	as Debug

stage		= "Core.Lift.LiftLambdas"
debug		= True
trace ss x	= if debug then Debug.trace (pprStrPlain ss) x else x


-- | Lift out all lambda bindings from this super.
--   	return the new super, as well as the lifted bindings as new supers.
--
--   XVars for the supers are left in-place of the lifted bindings.
--	After calling liftLambdasP we still need to go back and rewrite XVars
--	to calls to the new supers (passing in any free variables).
--
liftLambdasP 
	:: Top	
	-> LiftM 
		( Top		-- new super
		, [Top])	-- lifted bindings

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
	-- Decend into the first set of lambdas, until we get to the body of the function.
	-- We also collect up more-than constrains when we see them, as we'll need them
	-- when we add type-parameters to the lifted functions.
	| XLAM b k x1		<- x
	= do	x1'	<- lambdaLiftX superName (slurpBMore vtMore b) x1
		return	$ XLAM b k x1'	

	| XLam v t e eff clo	<- x
	= do	e'	<- lambdaLiftX superName vtMore e
		return	$ XLam v t e' eff clo
	
	-- walk over the body and chop out any inner functions.
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


-- | If this statement is a function then convert it into a new supercombinator,
--   add it to the lift state, and replace the RHS by a reference to this new super.
--
chopInnerS 
	:: Var		-- ^ This name of the top-level binding that this statement is a part of.
			--      This is to give lifted functions nicer names.
	-> Map Var Type	-- ^ A table of :> constraints on type variables.
			--      We need this when we re-quantify free type vars on lifted supers.
	-> Stmt	
	-> LiftM Stmt

chopInnerS topName vtMore s
 = case s of
 	SBind v x
	 | isFunctionX x
	 , not $ hasEmbeddedLambdasX x
	 -> 	chopInnerS2 topName vtMore s
	 
	_ -> 	return	s

-- | Chop out this function.
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
	(pSuper_lifted@(PBind _ x_lifted), freeVKs, freeVTs)	
			<- bindFreeVarsP vtMore pSuper

	-- add the new super to the lift state
	addChopped v vSuper pSuper_lifted

	-- work out the type of the new super.
	let tSuper	= checkedTypeOfExp (stage ++ ".chopInnerS2") x_lifted

	trace 	( "chopInnerS2: new super\n"
		% " pSuper_lifted:\n" 	%> pSuper_lifted	% "\n\n"
		% " superType:\n" 	%> tSuper		% "\n\n")
		$ return ()
		
	-- build the call to the new super.
	-- We only have to apply type args, and args binding the free variables.
	let typeArgs	= map (Right . makeSuperArgK)      freeVKs
	let valueArgs	= map (\(v, t) -> Left (XVar v t)) freeVTs
	let Just xCall	= buildApp (Left (XVar vSuper tSuper) : typeArgs ++ valueArgs)

	-- return the new binding
	return	$ SBind (Just v) xCall

	
-- | When we pass args that were free in a lambda abs back to the super, 
--   just pass new witness instead of their args. The core type checker can thread
--   the real ones through later on.
--
makeSuperArgK :: (Bind, Kind) -> Type
makeSuperArgK (b, k)
	| KApp{}	<- k
	, Just t	<- inventWitnessOfKind k
	= t
	
	| BVar v	<- b
	= TVar k $ UVar v
	
	| BMore v t	<- b
	= TVar k $ UMore v t


-- BindFree ---------------------------------------------------------------------------------------
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
			= checkExp x (envEmpty (stage ++ ".bindFreeVarsP"))

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
		$ filter (\c -> case takeTFree c of
					Just (v', _)	-> v /= v
					_		-> True)
		$ flattenTSum clo 

   in	(XLam v t x tPure clo', clo')


