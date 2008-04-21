
module Desugar.ToCore
	( toCoreTree
	, toCoreP
	, toCoreX )

where

-----
import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Debug.Trace	as Debug
import Util

-----
import Shared.Var		(Var, NameSpace(..))
import qualified Shared.Var 	as Var

import Shared.VarPrim
import Shared.Pretty
import Shared.Error
import qualified Shared.Exp		as S
import qualified Shared.Literal		as S


import qualified Type.Exp		as T
import Type.ToCore			(toCoreT, toCoreK)

import qualified Core.Util.Pack		as C
import qualified Core.Exp 		as C
import qualified Core.Util		as C
import qualified Core.Pretty		as C

import Desugar.ToCore.Base
import Desugar.ToCore.Lambda
import qualified Desugar.Pretty		as D
import Desugar.Project			(ProjTable)
import qualified Desugar.Exp 		as D
import qualified Desugar.Plate.Trans	as D
import qualified Desugar.Bits		as D
import qualified Desugar.Slurp.Util	as D

-----
stage	= "Desugar.ToCore"
debug	= False
trace ss x	
	= if debug 
		then Debug.trace (pprStrPlain ss) x
		else x

-- Tree --------------------------------------------------------------------------------------------
toCoreTree
	:: Map Var Var					-- ^ value -> type vars
	-> Map Var T.Type				-- ^ inferred type schemes
	-> Map Var (T.InstanceInfo T.Type T.Type)	-- ^ instantiation info
	-> Map Var (T.Kind, Maybe T.Type)		-- ^ the vars that were quantified during type inference
	-> ProjTable
	-> Map Var Var					-- ^ how to resolve projections
	-> D.Tree Annot
	-> C.Tree

toCoreTree	
	sigmaTable
	typeTable
	typeInst
	quantVars
	projTable
	projResolve
	sTree

 = 	cTree			
 where
	initCoreS'
		= initCoreS
		{ coreSigmaTable	= sigmaTable
		, coreMapTypes		= typeTable
		, coreMapInst		= typeInst
		, coreQuantVars		= quantVars
		, coreProjTable		= projTable  
		, coreProjResolve	= projResolve }
		
	mTree	= evalState 
			(toCoreTreeM sTree) 
			initCoreS'

	cTree	= mTree

toCoreTreeM tree
	= liftM concat
	$ mapM toCoreP tree
	
-- Top ---------------------------------------------------------------------------------------------
-- | Convert a top level thing to core.
toCoreP	:: D.Top Annot	
	-> CoreM [C.Top]

toCoreP	p
-- = trace ("toCoreP")
 = case p of
	D.PNil
	 ->	return []

	D.PExtern _ v tv (Just to)
	 -> do
		-- External types may contain monomorphic variables who's names have changed during 
		--	constraint solving. 
		let tv'	= toCoreT tv
		let to'	= toCoreT to
		
		return	$ [C.PExtern v tv' to']

	D.PData _ v vs ctors
	 -> do
		ctors'		<- mapM toCoreCtorDef ctors

		let d		= C.PData v vs ctors'
		cs		<- mapM (makeCtor primTObj v vs) ctors
		return (d:cs)

	D.PEffect _ v k
	 -> do
		let k'	= toCoreK k
		let p	= C.PEffect v k'
	 	return	[p]

	D.PBind nn (Just v) x
	 -> do	Just (C.SBind (Just v) x') 
			<- toCoreS (D.SBind nn (Just v) x)

		return	[C.PBind v x']

	D.PSig{}	-> return []
	D.PImport{}	-> return []


	-- classes
	D.PRegion _ v 
	 -> 	return	[ C.PRegion v [] ]


	D.PClass _ v k
	 -> 	return	[ C.PClass v (toCoreK k)]

	D.PClassDict _ v cts context sigs
	 -> do	let (vs, ts)	= unzip sigs
	 	let ts'		= map toCoreT ts
		let sigs'	= zip vs ts'

		let cts'	= map toCoreT cts

		return		$ [C.PClassDict v cts' [] sigs']

	-- type class instance
	D.PClassInst _ vClass cts context ss
	 -> do
		-- rewrite all the bindings in this instance
		-- The right hand side of the bindings must be a var.
		--	This should be enforced by Desugar.Project.
		let rewriteInstS s
			| D.SBind _ (Just v1) (D.XVarInst _ v2)	<- s
			= do	Just t2	<- lookupType v2
				return (v1, C.XVar v2 t2)
			
			| D.SBind _ (Just v1) (D.XVar     _ v2) <- s
			= do	Just t2	<- lookupType v2
				return (v1, C.XVar v2 t2)
			
			| otherwise
			= panic stage
				$  "toCoreP: RHS of projection dict must be a var\n"
				%  "    stmt:\n" %> s	% "\n"
	
		ss'	<- mapM rewriteInstS ss

		-- rewrite the context types
		let cts'	= map toCoreT cts

		return	$ [C.PClassInst vClass cts' [] ss']


	-- projections
	D.PProjDict{}	-> return []

	_ -> panic stage
		$ "toCoreP: no match for " % show p % "\n"
	

-- CtorDef -----------------------------------------------------------------------------------------
toCoreCtorDef	
	:: D.CtorDef Annot
	-> CoreM C.CtorDef
		
toCoreCtorDef	(D.CtorDef _ v fieldDefs)
 = do 	fieldDefs'	<- mapM toCoreFieldDef fieldDefs
	return	$ C.CtorDef v fieldDefs'

toCoreFieldDef	df
 = do	return	$ S.DataField 
		{ S.dPrimary	= S.dPrimary df
		, S.dLabel	= S.dLabel   df
		, S.dType	= toCoreT $ S.dType df

		, S.dInit	= case S.dInit df of
					Nothing			-> Nothing
					Just (D.XVarInst _ v)	-> Just v }
-- Make a constructor function
makeCtor 
	:: Var 
	-> Var 			-- data type name
	-> [Var] 		-- data type args
	-> D.CtorDef Annot	-- the ctor definition
	-> CoreM C.Top

makeCtor    objVar vData vsData (D.CtorDef _ ctorVar dataFields)
 = do
	let argTs	
		= map toCoreT
		$ map S.dType
		$ filter (\df -> S.dPrimary df) 
		$ dataFields

	tv	<- liftM toCoreT 
		$ D.makeCtorType newVarN vData vsData ctorVar dataFields

	let to	= C.unflattenFun (replicate (length argTs + 1) 
		$ (C.makeTData objVar C.KValue []))

	return	$ C.PCtor ctorVar tv to


-- Stmt --------------------------------------------------------------------------------------------
-- | Statements
toCoreS	:: D.Stmt Annot	
	-> CoreM (Maybe C.Stmt)
		
toCoreS (D.SBind _ Nothing x)
 = do	
 	x'		<- toCoreX x
	returnJ	$ C.SBind Nothing x'


toCoreS (D.SBind _ (Just v) x) 
 = do	
	-- lookup the generalised type of this binding.
	Just tScheme	<- lookupType v

 	-- convert the RHS to core.
	xCore		<- toCoreX x

	-- Add type lambdas and contexts
	xLam		<- fillLambdas v tScheme xCore

	returnJ 	$ C.SBind (Just v) 
			$ C.dropXTau xLam Map.empty tScheme


toCoreS	D.SSig{}
 	= return Nothing


-- Exp ---------------------------------------------------------------------------------------------
-- | Expressions
toCoreX	:: D.Exp Annot -> CoreM C.Exp
toCoreX xx
-- = trace ("toCoreX: " % xx % "\n")
 = case xx of

	D.XLambdaTEC 
		_ v x (T.TVar T.KValue vTV) eff clo
	 -> do	
		-- Only keep effect and closure bindings which are not quantified so they don't
		--	conflict with constraints on /\ bound vars.
		--
		-- Strip contexts off argument types, if we need the associated witnesses then these
		--	will be passed into the outer function.
		--
		Just tArg1	<- lookupType vTV
		let (argQuant, argFetters, _, argShape)
				= C.stripSchemeT tArg1

		portVars	<- gets coreQuantVars
		let fsWhere	= [ C.FWhere v t	
					| C.FWhere v t	<- argFetters
					, not $ Map.member v portVars]

		let fsMore	= [ f	| f@(C.FMore v t) <- argFetters ]
		
		let tArg	
			= trace 
				( "toCoreX:\n"
				% "    vTV     " % vTV		% "\n"
				% "    tArg1   " % tArg1 	% "\n"
				% "    fsWhere " % fsWhere	% "\n"
				% "    fsMore  " % fsMore	% "\n")
				$ C.packT
				$ C.buildScheme
					argQuant
					(fsWhere ++ fsMore)
					[]
					argShape

		-- If the effect/closures were vars then look them up from the graph
		effAnnot	<- loadEffAnnot $ toCoreT eff
		cloAnnot	<- loadCloAnnot $ toCoreT clo
				
		x'		<- toCoreX x
		
		return		
		 $ C.XLam 	v tArg
				x'
				(C.packT $ effAnnot)
				(C.packT $ cloAnnot)


	D.XApp	_ x1 x2
	 -> do
	 	x1'	<- toCoreX x1
		x2'	<- toCoreX x2
		return	$ C.XApp x1' x2' C.pure


	-- case match on a var
	D.XMatch _ (Just x@(D.XVar aObj varX)) alts
	 -> do
		Just tVar	<- lookupAnnotT aObj
		alts'		<- mapM (toCoreA (Just (varX, tVar))) alts
		
		return	$ C.XDo	[ C.SBind Nothing (C.XMatch alts') ]

	-- case match on an exp
	D.XMatch _ (Just x) alts
	 -> do
		let aX		=  D.getAnnotX x
		Just tX		<- lookupAnnotT aX

		x'	<- toCoreX x
		varX	<- newVarN NameValue
		
		alts'	<- mapM (toCoreA (Just (varX, tX))) alts
		
		return	$ C.XDo	[ C.SBind (Just varX) x'
				, C.SBind Nothing (C.XMatch alts') ]
			
	-- regular  match
	D.XMatch _ Nothing alts
	 -> do	alts'	<- mapM (toCoreA Nothing) alts
		
		return	$ C.XDo	[ C.SBind Nothing (C.XMatch alts') ]
		
	-- primitive constants
	D.XConst (Just (T.TVar T.KValue vT, _)) 
		cc@(S.CConst lit)
	 -> do	
	 	Just t		<- lookupType vT
	 	let t_flat	= C.stripContextT $ C.flattenT t
		
		return		$ toCoreConst t_flat cc

	D.XConst
		(Just (T.TVar T.KValue vT, _))
		cc@(S.CConstU lit)
	 -> do	
	 	Just t		<- lookupType vT
		let t_flat	= (C.stripContextT . C.flattenT) t

		return		$ toCoreConst t_flat cc

 
	-- We need the last statement in a do block to be a non-binding because of an
	--	interaction between the way we annotate generalised schemes with their types.
	--
	-- 	In this example:
	--		f () = do { a1 = 2; g () = a1 + 3; };
	--
	-- 	We infer:
	--          f :: forall %r1 %r2. () -> () -($c1)> Int %r1
	--   	      :- $c1 = a1 :: Int %r2
	--
	-- 	But in the core we reconstruct:
	--          g :: forall %r1. () -($c1)> Int %r1
	--            :- $c1 = a :: Int %r2
	--      and
	--          f :: forall %r2. () -> forall %r1. -($c1)> Int %r1
	--
	--	Forcing the last element of the XDo to be a value, ie
	--		f () = do { a1 = 2; g () = a1 + 3; g; }
	--	provides an instantiation of g, so its no longer directly quantified.
	--
	--	We could perhaps make the ToCore transform cleverer, but forcing the last statement
	--	to be a value seems a reasonable request, so we'll just run with that.
	--
	D.XDo 	_ stmts
	 -> do	stmts'	<- liftM catMaybes
	 		$  mapM toCoreS stmts
	
		case takeLast stmts' of
		 Just stmt@(C.SBind (Just _) _)
		  -> panic stage
		  	$ "toCoreX: last statement of a do block cannot be a binding.\n"
			% "    offending statement:\n"	%> stmt	% "\n"
		 
		 _ -> return	$ C.XDo stmts'
			
			
	D.XIfThenElse _ e1 e2 e3
	 -> do
		v	<- newVarN NameValue
		let aX	=  D.getAnnotX e1
		Just tX	<- lookupAnnotT aX

		e1'	<- toCoreX e1
		e2'	<- toCoreA (Just (v, tX)) (D.AAlt Nothing [D.GCase Nothing (D.WConLabel Nothing primTrue  [])] e2)
		e3'	<- toCoreA (Just (v, tX)) (D.AAlt Nothing [D.GCase Nothing (D.WConLabel Nothing primFalse [])] e3)
		
		return	$ C.XDo	[ C.SBind (Just v) e1'
				, C.SBind Nothing (C.XMatch [ e2', e3' ]) ]
	
	-- projections
	D.XProjTagged 
		(Just 	( T.TVar T.KValue vT
			, T.TVar T.KEffect vE))
		vTag x2 j
	 -> do
		x2'		<- toCoreX x2
		j'		<- toCoreJ j

		-- lookup the var for the projection function to use
		projResolve	<- gets coreProjResolve
		
		let Just vProj	= Map.lookup vTag projResolve
		
{-		trace 	( "XProjTagged\n"
			% "    vTag  = " % vTag		% "\n"
			% "    vProj = " % vProj	% "\n")
			$ return ()
-}
		x1'	<- toCoreVarInst vProj vTag
			
		return	$ C.XApp x1' x2' C.pure


	D.XProjTaggedT
		(Just 	( T.TVar T.KValue vT
			, T.TVar T.KEffect vE))
		vTag j
	 -> do
		-- lookup the var for the projection function to use
		projResolve	<- gets coreProjResolve
		let Just vProj	= Map.lookup vTag projResolve
		
{-		trace 	( "XProjTaggedT\n"
			% "    vTag  = " % vTag		% "\n"
			% "    vProj = " % vProj	% "\n")
			$ return ()
-}
		x1'	<- toCoreVarInst vProj vTag
		return	$ x1' 



	-- variables
	D.XVarInst 
		(Just (T.TVar T.KValue vT, _))
		v
	 -> 	toCoreVarInst v vT

	_ 
	 -> panic stage
		$ "toCoreX: cannot convert expression to core.\n" 
		% "    exp = " %> (D.transformN (\a -> (Nothing :: Maybe ())) xx) % "\n"


-- Lit ---------------------------------------------------------------------------------------------
-- | Convert a source constant to core
--	We don't know what implementation type a source literal is supposed to be until 
--	we do the type checking, so we need its type to guide the conversion. 
--	eg literal 5.0 could be either Float32 or Float64
--
--	In addition, the desugared language supports boxed and unboxed literals, but the core
--	only supports unboxed. The caller needs to handle this when using this function.

toCoreLit 
	:: C.Type 
	-> S.Literal 
	-> ( C.Lit	-- the converted literal
	   , Bool)	-- whether is was boxed

toCoreLit tLit lit
	= toCoreLit' (C.stripToShapeT tLit) lit
	
toCoreLit' tLit lit

	-- integers
	| S.LInt i		<- lit
	, Just (v, _, _)	<- C.takeTData tLit
	= case Var.bind v of
		Var.TInt8U	-> (C.LInt8  $ fromIntegral i, False)
		Var.TInt16U	-> (C.LInt16 $ fromIntegral i, False)
		Var.TInt32U	-> (C.LInt32 $ fromIntegral i, False)
		Var.TInt64U	-> (C.LInt64 $ fromIntegral i, False)

		Var.TInt8	-> (C.LInt8  $ fromIntegral i, True)
		Var.TInt16	-> (C.LInt16 $ fromIntegral i, True)
		Var.TInt32	-> (C.LInt32 $ fromIntegral i, True)
		Var.TInt64	-> (C.LInt64 $ fromIntegral i, True)
		

	-- floats
	| S.LFloat f		<- lit
	, Just (v, _, _)	<- C.takeTData tLit
	= case Var.bind v of
		Var.TFloat32U	-> (C.LFloat32 $ (fromRational . toRational) f, False)
		Var.TFloat64U	-> (C.LFloat64 $ (fromRational . toRational) f, False)

		Var.TFloat32	-> (C.LFloat32 $ (fromRational . toRational) f, True)
		Var.TFloat64	-> (C.LFloat64 $ (fromRational . toRational) f, True)

	-- chars
	| S.LChar c		<- lit
	, Just (v, _, _)	<- C.takeTData tLit
	= case Var.bind v of
		Var.TChar32U	-> (C.LChar32 c, False)
		Var.TChar32	-> (C.LChar32 c, True)
	
	-- strings
	| S.LString s		<- lit
	, Just (v, _, _)	<- C.takeTData tLit
	= case Var.bind v of
		Var.TStringU	-> (C.LString s, False)
		Var.TString	-> (C.LString s, True)


-- Const -------------------------------------------------------------------------------------------
-- | Convert a constant to core expression
--	All literals in the core are unboxed.
--	Perhaps literals in the desugared source should be unboxed as well.
--	TODO: merge this into toCoreLit
toCoreConst :: C.Type -> S.Const	-> C.Exp
toCoreConst tt const
 	= toCoreConst' (C.stripToShapeT tt) const

toCoreConst' tt const

	-- unboxed integers
	| S.CConstU lit			<- const
	, S.LInt i			<- lit
	, Just (v, k, [])		<- C.takeTData tt
	= case Var.bind v of
		Var.TInt8U	-> C.XLit $ C.LInt8  $ fromIntegral i
		Var.TInt16U	-> C.XLit $ C.LInt16 $ fromIntegral i
		Var.TInt32U	-> C.XLit $ C.LInt32 $ fromIntegral i
		Var.TInt64U	-> C.XLit $ C.LInt64 $ fromIntegral i
	

	-- boxed integers
	| S.CConst lit				<- const
	, S.LInt i				<- lit
	, Just (v, k, [r@(C.TVar C.KRegion _)])	<- C.takeTData tt
	= case Var.bind v of
		Var.TInt8	-> C.XPrim C.MBox [C.XType r, C.XLit $ C.LInt8  $ fromIntegral i]
		Var.TInt16	-> C.XPrim C.MBox [C.XType r, C.XLit $ C.LInt16 $ fromIntegral i]
		Var.TInt32	-> C.XPrim C.MBox [C.XType r, C.XLit $ C.LInt32 $ fromIntegral i]
		Var.TInt64	-> C.XPrim C.MBox [C.XType r, C.XLit $ C.LInt64 $ fromIntegral i]

	-- unboxed floats
	| S.CConstU lit			<- const
	, S.LFloat f			<- lit
	, Just (v, k, [])		<- C.takeTData tt
	= case Var.bind v of
		Var.TFloat32U	-> C.XLit $ C.LFloat32 $ (fromRational . toRational) f
		Var.TFloat64U	-> C.XLit $ C.LFloat64 $ (fromRational . toRational) f

	-- boxed floats
	| S.CConst lit				<- const
	, S.LFloat f				<- lit
	, Just (v, k, [r@(C.TVar C.KRegion _)])	<- C.takeTData tt
	= case Var.bind v of
		Var.TFloat32	-> C.XPrim C.MBox [C.XType r, C.XLit $ C.LFloat32 $ (fromRational . toRational) f]
		Var.TFloat64	-> C.XPrim C.MBox [C.XType r, C.XLit $ C.LFloat64 $ (fromRational . toRational) f]

	-- boxed chars
	| S.CConst lit	<- const
	, S.LChar s	<- lit
	, Just (v, k, [r@(C.TVar C.KRegion _)])	<- C.takeTData tt
	, Var.bind v == Var.TChar32
	= C.XPrim C.MBox [C.XType r, C.XLit $ C.LChar32 s]

	
	-- boxed strings
	| S.CConst lit	<- const
	, S.LString s	<- lit
	, Just (v, k, [r@(C.TVar C.KRegion _)])	<- C.takeTData tt
	, Var.bind v == Var.TString
	= C.XPrim C.MBox [C.XType r, C.XAPP (C.XLit $ C.LString s) r]
	

	| otherwise
	= panic stage
		$ "toCoreConst: no match\n"
		% "   tConst = " % show tt	% "\n"
		% "   const  = " % show const	% "\n"

-- VarInst -----------------------------------------------------------------------------------------
toCoreVarInst :: Var -> Var -> CoreM C.Exp
toCoreVarInst v vT
-- = trace ("toCoreVarInst" % v <> vT % "\n")
 = do
	Just tScheme	<- lookupType v
	mapInst		<- gets coreMapInst

	let (btsForall, _, ksContextC, tShape)
			= C.stripSchemeT tScheme
		
	-- TODO: break this out into a separate fn
	-- tag var with its type
	-- apply type args to scheme, add witness params

	-- lookup how this var was instantiated
	let Just instInfo = Map.lookup vT mapInst

	-- check how this var was instantiated to work out if we
	--	need to pass type args.
	case instInfo of

	 -- use of a lambda bound variable.
	 -- 	only rank1 polymorphism => lambda bound vars have monotypes
	 T.InstanceLambda vUse vBind _
	  -> do	{-trace 	( "varInst: TInstanceLambda\n"
	  		% "    vUse    = " % vUse	% "\n"
			% "    tScheme = " % tScheme	% "\n"
			% "    tShape  = " % tShape	% "\n")
			$ return ();
		 -}
	  	return $ C.XVar v tShape

	 -- non-recursive use of a let bound variable 
	 -- 	pass in the type args corresponding to the instantiated foralls.
	 T.InstanceLet vUse vBind tsInst _
	  -> do	
		-- Convert the type arguments to core.
		let tsInstC	= map toCoreT tsInst
			
		-- If the function being instantiated needs some context then there'll be a 
		--	separate witness for it... therefore we can safely erase contexts on
		--	type arguements for the instantiation.
		let tsInstCE	= map C.stripContextT tsInstC
			
		let tsInstC_packed	= map C.packT tsInstCE
			
		-- Work out what types belong to each quantified var in the type
		--	being instantiated.			
		let tsSub	= Map.fromList $ zip (map (C.varOfBind . fst) btsForall) tsInstC_packed

		-- If this function needs a witnesses we'll just make them up.
		--	Real witnesses will be threaded through in a later stage.
		let ksContextC'	= map (C.substituteT tsSub) ksContextC

		let tsContextC' = map C.packT
				$ map (\k -> case k of
						C.KClass v ts	-> C.TClass v ts) 
				$ ksContextC'

{-		trace ("varInst: "
			% vT 				% "\n"
			% "    tScheme         =\n" %> tScheme 		% "\n\n"
			% "    ksContext       = " % ksContextC		% "\n"
			% "    tsInstC         = " % tsInstC            % "\n"
			% "    tsInstCE        = " % tsInstCE		% "\n"
			% "    tsInstC_packed  = " % tsInstC_packed	% "\n"
			% "    tsSub           = " % tsSub 		% "\n"
			% "    tsContestC'     = " % tsContextC' 	% "\n")
			$ return ()
-}
		let Just xResult = 
			C.buildApp (C.XVar v tScheme : map C.XType (tsInstC_packed ++ tsContextC'))

		return	$ xResult

	 -- recursive use of a let-bound variable
	 -- 	pass the args on the type scheme back to ourselves.
	 T.InstanceLetRec vUse vBind (Just tSchemeT)
	  -> do
		let tSchemeC			= toCoreT tSchemeT
		let (tsReplay, ksContext)	= C.slurpForallContextT tSchemeC

		let tsContext	= map (\k -> case k of
						C.KClass v ts	-> C.TClass v ts)
				$ ksContext

		let Just xResult =
			C.buildApp (C.XVar v tSchemeC : map C.XType (tsReplay ++ tsContext))

		return $ xResult
			 

-- Proj --------------------------------------------------------------------------------------------
-- | Field porjections
toCoreJ :: D.Proj Annot -> CoreM C.Proj
toCoreJ jj
 = case jj of
	D.JField _ v	-> return $ C.JField v 	
	D.JFieldR _ v	-> return $ C.JFieldR v


-- Alt ---------------------------------------------------------------------------------------------
-- | Case Alternatives
toCoreA	:: Maybe (Var, C.Type)
	-> D.Alt Annot -> CoreM C.Alt
		
toCoreA mObj alt
 = case alt of
	D.AAlt _ gs x
	 -> do	
	 	gs'	<- mapM (toCoreG mObj) gs
		x'	<- toCoreX x
		
		return	$ C.AAlt gs' x'
	
	
-- Guards ------------------------------------------------------------------------------------------
-- | Guards
toCoreG :: Maybe (Var, C.Type)
	-> D.Guard Annot
	-> CoreM C.Guard

toCoreG mObj gg
	| D.GCase _ w		<- gg
	, Just (objV, objT)	<- mObj
	= do	(w', _)		<- toCoreW w
	 	return		$ C.GExp w' (C.XVar objV objT)
		
	| D.GExp _ w x		<- gg
	= do	(w', mustUnbox)	<- toCoreW w
	 	x'		<- toCoreX x
		
		-- All literal matching in core is unboxed, so we must unbox the match object if need be.
		case mustUnbox of
		 Just r		-> return $ C.GExp w' (C.XPrim C.MUnbox [C.XType r, C.XPrim C.MForce [x']])
		 Nothing	-> return $ C.GExp w' x'


-- Patterns ----------------------------------------------------------------------------------------
-- | Patterns
toCoreW :: D.Pat Annot
	-> CoreM 
		( C.Pat
		, Maybe C.Type)	-- whether to unbox the RHS, and from what region
	
toCoreW ww
 = case ww of
 	D.WConLabel 
		_ 
		v lvs
	 -> do 
	 	lvts		<- mapM toCoreA_LV lvs
	 	return	( C.WCon v lvts
			, Nothing)
	 
	-- match against boxed literal
	D.WConst 
		(Just	( T.TVar T.KValue vT
			, _))
		(S.CConst lit)
		
	 -> do	mT		<- liftM (liftM C.stripToShapeT)
	 			$  lookupType vT
	 
	 	let Just tLit		= mT
		let Just (_, _, r : _)	= C.takeTData tLit
	 
		let (lit', True)	= toCoreLit tLit lit
	 	return	( C.WLit lit'
			, Just r)
	
	-- match against unboxed literal
	D.WConst 
		(Just 	( T.TVar T.KValue vT
			, _ )) 
		(S.CConstU lit)

	 -> do	mT		<- liftM (liftM C.stripToShapeT)
	 			$  lookupType vT

	 	let Just tLit		= mT

		let (lit', False)	= toCoreLit tLit lit
	 	return	( C.WLit lit'
			, Nothing)

	D.WVar (Just 	(T.TVar T.KValue vT
			, _))
		var
	 -> do	
	 	return	( C.WVar var
			, Nothing)

	 
	_ -> panic stage
		$ "tCoreW: no match for " % show ww % "\n"
	 

toCoreA_LV (D.LIndex nn i, v)
 = do	Just t		<- lookupType v
 	let t_flat	=  (C.stripContextT . C.flattenT) t
	return	(C.LIndex i, v, t_flat)

toCoreA_LV (D.LVar nn vField, v)
 = do	Just t		<- lookupType v
 	let t_flat	= (C.stripContextT . C.flattenT) t
 	return	(C.LVar vField, v, t_flat)



