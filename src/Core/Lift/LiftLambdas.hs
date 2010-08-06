
module Core.Lift.LiftLambdas
	( liftLambdasP )
where
import Core.Util
import Core.Plate.Trans
import Core.Plate.FreeVarsXT
import Core.Lift.Base
import Util
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Core.Exp
import DDC.Core.Glob
import DDC.Core.Lint.Env
import DDC.Core.Lint.Exp
import DDC.Type
import DDC.Var
import DDC.Type.ClosureStore		(ClosureStore)
import qualified DDC.Type.ClosureStore	as Clo
import qualified Data.Map		as Map
import qualified Debug.Trace		as Debug

stage		= "Core.Lift.LiftLambdas"
debug		= False
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
 	xChopped	<- lambdaLiftX superName x
	let pLifted	= PBind superName xChopped

	-- Load the new supers from the stage
	cs		<- getChopped
	let psChopped	= map t3_3 cs

 	return	(pLifted, psChopped)


lambdaLiftX superName x
	-- Decend into the first set of lambdas, until we get to the body of the function.
	-- We also collect up more-than constrains when we see them, as we'll need them
	-- when we add type-parameters to the lifted functions.
	| XLAM b k x1		<- x
	= do	x1'	<- lambdaLiftX superName x1
		return	$ XLAM b k x1'	

	| XLam v t e eff clo	<- x
	= do	e'	<- lambdaLiftX superName e
		return	$ XLam v t e' eff clo
	
	-- walk over the body and chop out any inner functions.
	| otherwise
	= do	let table	
			= transTableId 
				{ transS 	= chopInnerS superName
				, decendT	= False }

		transZM table x


-- | If this statement is a function then convert it into a new supercombinator,
--   add it to the lift state, and replace the RHS by a reference to this new super.
--
chopInnerS 
	:: Var		-- ^ Name of the top-level binding that this statement is a part of.
			--   Used give lifted functions nice names.
	-> Stmt	
	-> LiftM Stmt

chopInnerS topName s
 = case s of
 	SBind v x
	 | isFunctionX x
	 , not $ hasEmbeddedLambdasX x
	 -> 	chopInnerS2 topName s
	 
	_ -> 	return	s

-- | Chop out this function.
chopInnerS2 topName (SBind Nothing x)
 = do	v	<- newVar NameValue
	chopInnerS2 topName (SBind (Just v) x)

chopInnerS2 topName (SBind (Just v) x)
 = do	
	-- make a name for the new super
	vN		<- newVar NameValue
	let vSuper	=  vN { varName = (varName topName) ++ "_" ++ (varName vN) }

	-- turn this binding into a super-combinator by binding its free variables as new parameters.
	let pSuper	= PBind vSuper x
	(pSuper_lifted@(PBind _ x_lifted), freeVKs, freeVTs)	
			<- bindFreeVarsP pSuper

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
	let typeArgs	= map (Right . makeSuperArgK) freeVKs
	let valueArgs	= map Left                    freeVTs
	let Just xCall	= buildApp (Left (XVar vSuper tSuper) : typeArgs ++ valueArgs)

	-- return the new binding
	return	$ SBind (Just v) xCall

	
-- | When we pass args that were free in a lambda abs back to the super, 
--   just pass new witness instead of their args. The core type checker can thread
--   the real ones through later on.
--
makeSuperArgK :: Type -> Type
makeSuperArgK (TVar k b)
	| KApp{}	<- k
	, Just t	<- inventWitnessOfKind k
	= t
	
	| UVar v	<- b
	= TVar k $ UVar v
	
	| UMore v t	<- b
	= TVar k $ UMore v t


-- BindFree ---------------------------------------------------------------------------------------
-- | Turn this PBind into a combinator by binding its free variables as new parameters.
--   Returns the freshly bound vars and there type/kinds to help construct calls to it
bindFreeVarsP 
	:: Top
	-> LiftM 
		( Top
		, [Type]	--  type vars that were bound
		, [Exp])	-- value vars that were bound
		
bindFreeVarsP (PBind vTop xx)
 = do
	-- Work out the non-top-level vars that are free in the expresison.
	cgHeader	<- gets stateHeaderGlob
	cgModule	<- gets stateModuleGlob 

	let isLocalFree free
		| Just v	<- takeVarOfFree free
--		, elem (varNameSpace v) [NameValue, NameType]
		, not $ varIsBoundAtTopLevelInGlob cgModule v
		, not $ varIsBoundAtTopLevelInGlob cgHeader v
		= True
		
		| otherwise
		= False

	let (freesVal, freesType)
		= partition isFreeX 
		$ Map.elems 
		$ Map.filter isLocalFree 
		$ freeVarsXT xx	

 	-- Work out the type of the expression
	--	The effect should always be TBot because we only ever lift (value) lambda abstractions.
	--	The closure returned from reconX will be flat, ie a TSum of all the free vars
	let (xRecon, tRecon, _, cRecon) 
			= checkExp xx (envEmpty (stage ++ ".bindFreeVarsP"))

	-- Bind free value vars with new value lambdas.
	let Just freeXVars	= sequence $ map takeExpOfFree freesVal
	let (xBindVal, _) 	= foldr addLambda (xRecon, Clo.fromClosure cRecon) freeXVars

	-- Bind free type vars with new type lambdas
	let Just freeTVars 	= sequence $ map takeTypeOfFree freesType
	let xBindType		= foldr addLAMBDA xBindVal freeTVars

	-- make the top level binding
	let pBound		= PBind vTop xBindType

	trace
	 (pprStrPlain	$ "\n\n\n"
	 		% "* bindFreeVarsP\n"
			% "    vTop      = "	% vTop		% "\n"
			% "    x:\n"		%> xx		% "\n\n"
			% "    freeXVars:\n"    %> freeXVars	% "\n\n"
			% "    freeTVars:\n"	%> freeTVars	% "\n\n"
	 		% "    tRecon:\n"	%> tRecon	% "\n\n"
			% "    cRecon:\n"	%> cRecon	% "\n\n"
			% "    xBindVal:\n"	%> xBindVal	% "\n\n")
	 $ return 
	 	( pBound
	 	, freeTVars
		, freeXVars)


-- | Add a value lambda to the outside of this expression.
--	the effect  is set to TBot
addLambda :: Exp -> (Exp, ClosureStore) -> (Exp, ClosureStore)
addLambda (XVar v t) (x, clos)
 = let	-- filter out the closure term corresponding to the var that is bound by this lambda.
	clos'	= Clo.mask v clos

   in	( XLam v t x tPure (Clo.toClosure clos')
 	, clos')


-- | Add some type lambdas to an expression.	
addLAMBDA :: Type -> Exp -> Exp
addLAMBDA tt x
 = case tt of
	TVar k (UVar v)		-> XLAM (BVar v) k x
	TVar k (UMore v t)	-> XLAM (BMore v t) k x
	_			-> panic stage $ "addLAMBDA: no match"


