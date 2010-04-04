
module Desugar.ToCore
	( toCoreTree
	, toCoreP
	, toCoreX )
where
import Util
import Shared.VarPrim
import Desugar.ToCore.Base
import Desugar.ToCore.Lambda
import DDC.Base.SourcePos
import DDC.Base.DataFormat
import DDC.Base.Literal
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Var
import Shared.VarUtil			(isDummy, varPos)
import Type.ToCore			(toCoreT, toCoreK)
import Desugar.Pretty			()
import Desugar.Project			(ProjTable)
import qualified Shared.Exp		as S
import qualified Type.Exp		as T
import qualified Type.Util		as T
import qualified Core.Util.Pack		as C
import qualified Core.Exp 		as C
import qualified Core.Util		as C
import qualified Desugar.Exp 		as D
import qualified Desugar.Plate.Trans	as D
import qualified Desugar.Bits		as D
import qualified Desugar.Slurp.Util	as D
import qualified Data.Map		as Map
import qualified Debug.Trace		as Debug

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
 = -- trace ("toCoreP " % p) $ 
   case p of
	D.PNil
	 ->	return []

	D.PExtern _ v tv (Just to)
	 -> do
		-- External types may contain monomorphic variables who's names have changed during 
		--	constraint solving. 
		let tv'	= toCoreT tv
		let to'	= toCoreT to
		
		return	$ [C.PExtern v tv' to']

	D.PExternData _ s v k
	 -> do	return	$ [C.PExternData v k]

	D.PData _ vData vsParam ctors
	 -> do	ctors'		<- zipWithM 
					(toCoreCtorDef vData vsParam) 
					ctors
					[0 .. length ctors]
					
		let vsCtors	= map C.ctorDefName ctors'
		let mCtors	= Map.fromList $ zip vsCtors ctors'
		return	[C.PData vData mCtors]

	D.PBind nn (Just v) x
	 -> do	Just (C.SBind (Just v) x') 
			<- toCoreS (D.SBind nn (Just v) x)

		return	[C.PBind v x']

	D.PSig{}	-> return []
	D.PImport{}	-> return []


	-- classes
	D.PRegion _ v 
	 -> 	return	[ C.PRegion v [] ]

	D.PKindSig _ v k
	 | T.resultKind k == T.kValue
	 ->	return	[ C.PData   v Map.empty ]
	
	 | T.resultKind k == T.kEffect
	 ->	return	[ C.PEffect v (toCoreK k) ]
	
	 -- we could probably add the following, but we don't have test programs yet.
	 | otherwise
	 -> panic stage $ unlines
		[ "toCoreP: Type constructors that to not have a result kind"
		, "of either * or ! are not supported in the core langauge yet" ]

	D.PClass _ v s
	 -> 	return	[ C.PClass v s]

	D.PClassDict _ v cts sigs
	 -> do	let (vs, ts)	= unzip sigs
	 	let ts'		= map toCoreT ts
		let sigs'	= zip vs ts'

		let vks'	= map (\(T.TVar k v) -> (v, k))
				$ map toCoreT cts

		return		$ [C.PClassDict v vks' sigs']

	-- type class instance
	D.PClassInst _ vClass cts ss
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

		return	$ [C.PClassInst vClass cts' ss']


	-- projections
	D.PProjDict{}	-> return []

	_ -> panic stage
		$ "toCoreP: no match for " % show p % "\n"
	

-- CtorDef -----------------------------------------------------------------------------------------
-- | Convert a desugared data constructor definition to core form.
toCoreCtorDef	
	:: Var			-- ^ var of data type
	-> [Var]		-- ^ var of type params
	-> D.CtorDef Annot
	-> Int			-- ^ ctor tag
	-> CoreM C.CtorDef
		
toCoreCtorDef vData vsParam (D.CtorDef _ vCtor dataFields) tag
 = do 	tCtor	<- liftM toCoreT
		$  D.makeCtorType newVarN vData vsParam vCtor dataFields

	let fieldIndicies
		= takeFieldIndicies dataFields

	return	$ C.CtorDef vCtor tCtor (length dataFields) tag fieldIndicies


-- | For fields with a label, 
--	construct a map from the label var to the index of that field in the constructor.
--
takeFieldIndicies 
	:: [S.DataField (D.Exp Annot) T.Type] 	-- ^ fields of a data constructor
	-> Map Var Int

takeFieldIndicies dfs
 	= Map.fromList [ (v, i) | (Just v, i) <- zip (map S.dLabel dfs) [0..] ]
	


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
 = -- trace ("toCoreX: " % xx % "\n") $
   case xx of

	D.XLambdaTEC 
		_ v x (T.TVar kV vTV) eff clo
	 | kV == T.kValue
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
		let fsWhere	= [ T.FWhere t1 t2	
					| T.FWhere t1@(T.TVar _ v) t2	<- argFetters
					, not $ Map.member v portVars]

		let fsMore	= [ f	| f@(T.FMore v t) <- argFetters ]
		
		let tArg	
			= C.packT
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
		return	$ C.XApp x1' x2' T.tPure


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
	D.XLit (Just (T.TVar kV vT, _)) litfmt
	 | kV	== T.kValue
	 -> do	
	 	Just t		<- lookupType vT
	 	let t_flat	= C.stripContextT $ C.flattenT t
		
		return		$ toCoreXLit t_flat xx

 
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
		(Just 	( T.TVar kV vT
			, T.TVar kE vE))
		vTagInst vTagClo x2 j
	 | kV == T.kValue
	 , kE == T.kEffect
	 -> do
		x2'		<- toCoreX x2
		j'		<- toCoreJ j

		-- lookup the var for the projection function to use
		projResolve	<- gets coreProjResolve

		trace 	( "XProjTagged\n"
			% "    vTagInst  = " % vTagInst		% "\n")
			$ return ()
		
		let vProj
		 	= case Map.lookup vTagInst projResolve of
				Nothing	-> panic stage
					$ "No projection function for " % vTagInst % "\n"
					% " exp = " % xx % "\n\n"

				Just v	-> v
							
		x1'	<- toCoreVarInst vProj vTagInst
			
		return	$ C.XApp x1' x2' T.tPure

	D.XProjTaggedT
		(Just 	( T.TVar kV vT
			, T.TVar kE vE))
		vTagInst tTagClo j
	 | kV == T.kValue
	 , kE == T.kEffect
	 -> do
		-- lookup the var for the projection function to use
		projResolve	<- gets coreProjResolve
		let Just vProj	= Map.lookup vTagInst projResolve
		
		x1'	<- toCoreVarInst vProj vTagInst
		return	$ x1' 



	-- variables
	D.XVarInst 
		(Just (T.TVar kV vT, _))
		v
	 | kV == T.kValue
	 -> 	toCoreVarInst v vT

	_ 
	 -> panic stage
		$ "toCoreX: cannot convert expression to core.\n" 
		% "    exp = " %> (D.transformN (\a -> (Nothing :: Maybe ())) xx) % "\n"



-- toCoreLitX --------------------------------------------------------------------------------------
-- | Convert a literal to core
--	The desugared language supports boxed literals, but all literals in the core
--	should be unboxed.
--
toCoreXLit :: T.Type -> D.Exp Annot -> C.Exp
toCoreXLit tt xLit
 	= toCoreXLit' (C.stripToShapeT tt) xLit

toCoreXLit' tt xLit@(D.XLit n litfmt@(LiteralFmt lit fmt))

	-- raw unboxed strings need their region applied
	| LString _	<- lit
	, Unboxed	<- fmt
	= let	Just (_, _, [tR]) = T.takeTData tt
	  in	C.XAPP (C.XLit litfmt) tR

	-- other unboxed literals have kind *, so there is nothing more to do
	| dataFormatIsUnboxed fmt
	= C.XLit litfmt
	
	-- unboxed strings have kind % -> *, 
	--	so we need to apply the region to the unboxed literal
	--	when building the boxed version.
	| LString _	<- lit
	, Boxed	<- fmt
	= let	Just (_, _, [tR]) = T.takeTData tt
		Just fmtUnboxed		= dataFormatUnboxedOfBoxed fmt
	  in	C.XPrim C.MBox 
			[ C.XType tR
			, C.XAPP (C.XLit (LiteralFmt lit fmtUnboxed)) tR]


	-- the other unboxed literals have kind *, 
	--	so we can just pass them to the the boxing primitive directly.
	| Just (v, k, [tR]) <- T.takeTData tt
	= let	Just fmtUnboxed		= dataFormatUnboxedOfBoxed fmt
	  in	C.XPrim C.MBox 
			[ C.XType tR
			, C.XLit $ LiteralFmt lit fmtUnboxed]
		
	| otherwise
	= panic stage
		$ "toCoreLitX: no match\n"
		% "   tLit   = " % show tt	% "\n"
		% "   xLit   = " % show xLit	% "\n"
	

-- VarInst -----------------------------------------------------------------------------------------
toCoreVarInst :: Var -> Var -> CoreM C.Exp
toCoreVarInst v vT
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
		let tsSub	= Map.fromList $ zip (map (T.varOfBind . fst) btsForall) tsInstC_packed

		-- If this function needs a witnesses we'll just make them up.
		--	Real witnesses will be threaded through in a later stage.
		let ksContextC'	= map (C.substituteT tsSub) ksContextC
		let tsContextC' = map C.packT
				$ map (\k -> let Just t = T.inventWitnessOfClass k in t)
				$ ksContextC'

		trace ("varInst: "
			% vT 				% "\n"
			% "    tScheme         =\n" %> tScheme 		% "\n\n"
			% "    ksContext       = " % ksContextC		% "\n"
			% "    tsInstC         = " % tsInstC            % "\n"
			% "    tsInstCE        = " % tsInstCE		% "\n"
			% "    tsInstC_packed  = " % tsInstC_packed	% "\n"
			% "    tsSub           = " % tsSub 		% "\n"
			% "    tsContestC'     = " % tsContextC' 	% "\n")
			$ return ()

		let Just xResult = 
			C.buildApp (C.XVar v tScheme : map C.XType (tsInstC_packed ++ tsContextC'))

		return	$ xResult

	 -- recursive use of a let-bound variable
	 -- 	pass the args on the type scheme back to ourselves.
	 T.InstanceLetRec vUse vBind (Just tSchemeT)
	  -> do
		let tSchemeC			= toCoreT tSchemeT
		let (tsReplay, ksContext)	= C.slurpForallContextT tSchemeC

		let tsContext	= map (\k -> let Just t = T.inventWitnessOfClass k in t)
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
toCoreA	:: Maybe (Var, T.Type)
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
toCoreG :: Maybe (Var, T.Type)
	-> D.Guard Annot
	-> CoreM C.Guard

toCoreG mObj gg
	| D.GCase _ w		<- gg
	, Just (objV, objT)	<- mObj
	= do	(w', mustUnbox)	<- toCoreW w

		let x		= C.XVar objV objT
		case mustUnbox of
		 Just r		-> return $ C.GExp w' (C.XPrim C.MUnbox [C.XType r, C.XPrim C.MForce [x]])
		 Nothing	-> return $ C.GExp w' x
		
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
		, Maybe T.Type)	-- whether to unbox the RHS, and from what region
	
toCoreW ww
	| D.WConLabel _ v lvs	<- ww
	= do	let vlist	= filter (not . isDummy) $ v : map snd lvs
		let spos	= if length vlist == 0
					then SourcePos ("?", 0, 0)
					else varPos $ head vlist 
		lvts		<- mapM toCoreA_LV lvs
		return	( C.WCon spos v lvts
			, Nothing)
	 
	-- match against a boxed literal
	--	All matches in the core language are against unboxed literals, 
	--	so we need to rewrite the literal in the pattern as well as the guard expression.
	| D.WLit (Just	( T.TVar kV vT
			, _))
		litFmt@(LiteralFmt lit fmt) <- ww
	, kV == T.kValue

	, dataFormatIsBoxed fmt
	= do	mT	<- liftM (liftM C.stripToShapeT)
	 		$  lookupType vT
	 
		-- work out what region the value to match against is in
		--	we pass this back up to toCoreG so it knows to do the unboxing.
	 	let Just tLit		= mT
		let Just (_, _, r : _)	= T.takeTData tLit

		-- get the unboxed version of this data format.
	 	let Just fmt_unboxed	= dataFormatUnboxedOfBoxed fmt
	
	 	return	( C.WLit (varPos vT) (LiteralFmt lit fmt_unboxed)
			, Just r)
	
	-- match against unboxed literal
	--	we can do this directly.
	| D.WLit
		(Just 	( T.TVar kV vT
			, _ )) 
		litFmt@(LiteralFmt lit fmt)	<- ww
	, kV == T.kValue
	, dataFormatIsUnboxed fmt
	= do	return	( C.WLit (varPos vT) litFmt
			, Nothing)


	-- match against a variable
	| D.WVar (Just 	(T.TVar kV vT
			, _))
		var		<- ww
	, kV == T.kValue
	= do
		return	( C.WVar var
			, Nothing)

	| otherwise
	= panic stage
		$ "tCoreW: no match for " % show ww % "\n"
	 

toCoreA_LV (D.LIndex nn i, v)
 = do	Just t		<- lookupType v
 	let t_flat	=  (C.stripContextT . C.flattenT) t
	return	(C.LIndex i, v, t_flat)

toCoreA_LV (D.LVar nn vField, v)
 = do	Just t		<- lookupType v
 	let t_flat	= (C.stripContextT . C.flattenT) t
 	return	(C.LVar vField, v, t_flat)



