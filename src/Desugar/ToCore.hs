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
import Shared.Error
import qualified Shared.Exp	as S
import qualified Shared.Literal	as S

import qualified Desugar.Exp 		as D
import qualified Desugar.Plate.Trans	as D

import qualified Type.Exp	as T
import Type.ToCore		(toCoreT, toCoreK)

import qualified Core.Exp 		as C
import qualified Core.Util		as C
import qualified Core.Pretty		as C
import qualified Core.Optimise.Boxing	as C	(unboxedType)

import Desugar.ToCore.Base
import Desugar.ToCore.Lambda
import Desugar.ToCore.Util
import Desugar.Pretty
import Desugar.Project		(ProjTable)


import qualified Debug.Trace	as Debug

-----
stage	= "Desugar.ToCore"

debug		= False
trace ss x	= if debug 
			then Debug.trace (pretty ss) x
			else x

-----------------------
toCoreTree
	:: (Map Var Var)				-- ^ value -> type vars
	-> (Map Var T.Type)				-- ^ inferred type schemes
	-> (Map Var (T.InstanceInfo T.Type T.Type))	-- ^ instantiation info
	-> (Set Var)					-- ^ the vars which are ports
	-> (Map Var (Map Var T.Type))			-- ^ port substitution
	-> ProjTable
	-> D.Tree Annot
	-> C.Tree

toCoreTree	
	sigmaTable
	typeTable
	typeInst
	quantVars
	portTable
	projTable
	sTree

 = 	cTree			
 where
	initCoreS'
		= initCoreS
		{ coreSigmaTable	= sigmaTable
		, coreMapTypes		= typeTable
		, coreMapInst		= typeInst
		, corePortVars		= quantVars
		, corePortTable		= Map.map (Map.map toCoreT) portTable
		, coreProject		= projTable }
		
	mTree	= evalState 
			(toCoreTreeM sTree) 
			initCoreS'

	cTree	= mTree

toCoreTreeM tree
	= liftM concat
	$ mapM toCoreP tree
	

flattenEs e
 = case e of
 	C.TNil			-> []
	C.TSum C.KEffect es	-> es
	e			-> [e]
	

-- | Convert a top level thing to core.
toCoreP	:: D.Top Annot	
	-> CoreM [C.Top]

toCoreP	p
 = case p of
	D.PNil
	 ->	return []

	D.PExtern _ v tv (Just to)
	 -> do
		let tv'	= toCoreT tv
		let to'	= toCoreT to
		
		return	$ [C.PExtern v tv' to']

	D.PData _ v vs ctors
	 -> do
		ctors'		<- mapM toCoreCtorDef ctors

		let d		= C.PData v vs ctors'
		let cs		= map (makeCtor primTObj v vs) ctors
		return (d:cs)

	D.PEffect _ v k
	 -> do
		let k'	= toCoreK k
		let p	= C.PEffect v k'
	 	return	[p]

	D.PBind nn (Just v) x
	 -> do	sigmaTable	<- gets coreSigmaTable
	 	let Just vT	= Map.lookup v sigmaTable
	 
	 	Just (C.SBind (Just v) x') 
			
			-- Rewrite the RHS of the binding while using the type substitution
			--	we got when we did the contra-variant port rewrite on the 
			--	generalised type of v.
			<- withPortSub vT 
			$ toCoreS (D.SBind nn (Just v) x)

		return	[C.PBind v x']

	D.PSig{}	-> return []
	D.PImport{}	-> return []


	-- classes
	D.PRegion _ v	
	 -> 	return	[ C.PRegion v ]


	D.PClass _ v k
	 -> 	return	[ C.PClass v (toCoreK k)]

	D.PClassDict _ v cts context sigs
	 -> do	let (vs, ts)	= unzip sigs
	 	let ts'		= map toCoreT ts
		let sigs'	= zip vs ts'

		let cts'	= map toCoreT cts

		return		$ [C.PClassDict v cts' [] sigs']

	D.PClassInst _ v cts context defs
	 -> do
		defs'		<- mapM (\(v, x)
					-> case x of
						D.XVarInst _ v2	-> return (v, C.XVar v2)
						D.XVar     _ v2 -> return (v, C.XVar v2))
					$ defs

		let cts'	= map toCoreT cts
		return	$ [C.PClassInst v cts' [] defs']

	-- projections
	D.PProjDict{}	-> return []

	_ -> panic stage
		$ "toCoreP: no match for " % show p % "\n"
	



-----
toCoreCtorDef	
	:: D.CtorDef Annot
	-> CoreM C.CtorDef
		
toCoreCtorDef	(D.CtorDef _ v fieldDefs)
 = do 	fieldDefs'	<- mapM toCoreFieldDef fieldDefs
	return	$ C.CtorDef v fieldDefs'

toCoreFieldDef	df
 = do
	cInit	<- case S.dInit df of
		    Just x 
		     -> do
		    	x'	<- toCoreX x
			return	$ Just (C.XDo [C.SBind Nothing x'])

		    Nothing -> return Nothing

	return	$ S.DataField 
		{ S.dPrimary	= S.dPrimary df
		, S.dLabel	= S.dLabel   df
		, S.dType	= toCoreT $ S.dType df
		, S.dInit	= cInit }
		

makeCtor 
	:: Var 
	-> Var 
	-> [Var] 
	-> D.CtorDef Annot
	-> C.Top

makeCtor    objVar dataVar ts (D.CtorDef _ ctorVar dataFields)
 = let
	argTs	= map toCoreT
		$ map S.dType
		$ filter (\df -> S.dPrimary df) 
		$ dataFields

	tv	= C.makeCtorTypeAVT argTs dataVar ts 

	to	= C.unflattenFun (replicate (length argTs + 1) 
		$ (C.TData objVar []))

   in   C.PCtor ctorVar tv to


-----------------------
-- Stmt
--
toCoreS	:: D.Stmt Annot	
	-> CoreM (Maybe C.Stmt)
		
toCoreS (D.SBind _ Nothing x)
 = do	
 	x'		<- toCoreX x
	returnJ	$ C.SBind Nothing x'


toCoreS (D.SBind _ (Just v) x) 
 = do	
	-- lookup the generalised type of this binding.
	tScheme		<- getType v

 	-- convert the RHS to core.
	xCore		<- toCoreX x

	-- Add type lambdas and contexts
	xLam		<- fillLambdas v tScheme xCore

	returnJ 	$ C.SBind (Just v) 
			$ dropXTau xLam [] tScheme


toCoreS	D.SSig{}
 	= return Nothing


-----------------------
-- Exp
--
toCoreX	:: D.Exp Annot -> CoreM C.Exp
toCoreX xx
 = case xx of

	D.XLambdaTEC 
		_ v x (T.TVar T.KData vTV) effVar cloVar
	 -> do	
		vT		<- liftM C.stripToShapeT $ getType vTV

		-- If the effect/closures were vars then look them up from the graph
		effLet	<- case effVar of
				T.TVar T.KEffect vE	
				 -> do	e	<- liftM C.stripToShapeT $ getType vE
				 	return	$ Just (vE, e)

				T.TBot T.KEffect	
				 -> 	return	$ Nothing
				
		cloLet	<- case cloVar of
				T.TVar T.KClosure vC	
				 -> do	c	<- liftM C.stripToShapeT $ getType vC
				 	return	$ Just (vC, c)
				 
				T.TBot T.KClosure 	
				 -> 	return	Nothing

		-- short out bottoms
		let botEC ec ml = 
			case (ec, ml) of
				(T.TVar T.KClosure v1, Just (v2, C.TBot C.KClosure))
				  | v1 == v2	-> (C.TBot C.KClosure, Nothing)

				(T.TVar T.KEffect v1, Just (v2, C.TBot C.KEffect))
				  | v1 == v2	-> (C.TBot C.KEffect, Nothing)

				_		-> (toCoreT $ ec, ml)
		
		let (eff, mEffLet)	= botEC effVar effLet
		let (clo, mCloLet)	= botEC cloVar cloLet

		x'	<- toCoreX x
		
		return	$ C.makeXTet (catMaybes [mEffLet, mCloLet])
			$ C.XLam v vT 
				x'
				eff clo


	D.XApp	_ x1 x2
	 -> do
	 	x1'	<- toCoreX x1
		x2'	<- toCoreX x2
		return	$ C.XApp x1' x2' C.pure


	-- case match on a var
	D.XMatch _ (Just x@(D.XVar _ varX)) alts
	 -> do
		alts'	<- mapM (toCoreA (Just varX)) alts
		
		return	$ C.XDo	[ C.SBind Nothing (C.XMatch alts' C.TNil) ]

	-- case match on an exp
	D.XMatch _ (Just x) alts
	 -> do
		x'	<- toCoreX x
		varX	<- newVarN NameValue
		
		alts'	<- mapM (toCoreA (Just varX)) alts
		
		return	$ C.XDo	[ C.SBind (Just varX) x'
				, C.SBind Nothing (C.XMatch alts' C.TNil) ]
			
	-- regular  match
	D.XMatch _ Nothing alts
	 -> do	alts'	<- mapM (toCoreA Nothing) alts
		
		return	$ C.XDo	[ C.SBind Nothing (C.XMatch alts' C.TNil) ]
		
	-- primitive constants
	D.XConst (Just (T.TVar T.KData vT, _)) 
		(S.CConst lit)

	 -> do
	 	t		<- getType vT
	 	case C.unboxedType t of
		 Just tU@(C.TData _ [tR])
		  -> return 	$ C.XPrim (C.MBox (C.stripToShapeT t) tU) [C.XConst (S.CConstU lit) tU] (C.TEffect primRead [tR])
			
		 Nothing
		  -> panic stage
		  	$ "toCoreX: no unboxed version for type " % t

	D.XConst
		(Just (T.TVar T.KData vT, _))
		(S.CConstU lit)
	 -> do	
	 	t	<- getType vT
		return	$  C.XConst (S.CConstU lit) t

 
	D.XDo 	_ stmts
	 -> do	stmts'	<- liftM catMaybes
	 		$  mapM toCoreS stmts

		return	$ C.XDo stmts'
			
			
	D.XIfThenElse _ e1 e2 e3
	 -> do
		v	<- newVarN NameValue
		e1'	<- toCoreX e1
		e2'	<- toCoreA (Just v) (D.AAlt Nothing [D.GCase Nothing (D.WConLabel Nothing primTrue  [])] e2)
		e3'	<- toCoreA (Just v) (D.AAlt Nothing [D.GCase Nothing (D.WConLabel Nothing primFalse [])] e3)
		
		return	$ C.XDo	[ C.SBind (Just v) e1'
				, C.SBind Nothing (C.XMatch [ e2', e3' ] C.TNil) ]
	
{-	
	D.XProj	(Just (T.TVar T.KData vT, T.TVar T.KEffect vE))
		x j
	 -> do
	 	(x', xT) 	<- toCoreX x
		jT		<- getType vT

		let (C.TData xV _)	= xT

		case j of
		 D.JField _ v	-> return 
		 			C.XPrim (C.MFun primProjField  jT) [C.XVar xV, C.XVar v, x'] C.TNil
					, jT)
		
		 D.JFieldR _ v	-> return
		 			( C.XPrim (C.MFun primProjFieldR jT) [C.XVar xV, C.XVar v, x'] C.TNil
					, jT)


	D.XProjTagged 
		(Just 	( T.TVar T.KData vT
			, T.TVar T.KEffect vE))
		vTag x j
	 -> do
		(x', xT@(C.TData vCon _))	
			<- toCoreX x

		resultT			<- getType vT
		mapProj			<- gets coreProject
	
		let (Just [(t', implList)])	
			= Map.lookup vCon mapProj

		let projName		= case j of
						D.JField  _ v	-> Var.name v
						D.JFieldR _ v	-> "ref_" ++ Var.name v


		let mVarImpl		= liftM snd
					$ find (\(v, _) -> Var.name v == projName)
					$ Map.toList implList

		let vImpl	
			= case mVarImpl of
				Just varImpl	-> varImpl
				Nothing		-> panic stage 
						$ "toCoreX: can't find projection implementation for " % j % "\n"
						% "   node         = " % xx 		% "\n"
						% "   object       = " % x		% "\n"
						% "   object type  = " % xT		% "\n"
						% "   projName     = " % projName	% "\n"
						% "   implList     = " % implList	% "\n"

		funX		<- addTypeApps vImpl vTag (C.XVar vImpl)

		return	( C.XApp funX x' C.TNil
			, resultT )
-}

{-
	D.XVar	(Just (T.TVar T.KData vT, _))
		v
	 -> do	
	 	t	<- getType vT
		return	( C.XVar v
	 		, t)

	D.XVar	Nothing v
	 -> do	return	( C.XVar v
	 		, C.TNil)
-}	
	D.XVarInst 
		(Just (T.TVar T.KData vT, _))
		v
	 -> do	
		tScheme		<- getType v
		mapInst		<- gets coreMapInst

		let (vtsForall, vtsWhere, tsContextC, tShape)
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
		  ->	return $ C.XVar v

		 -- non-recursive use of a let bound variable 
		 -- 	pass in the type args corresponding to the instantiated foralls.
		 T.InstanceLet vUse vBind tsInst _
		  -> do	
		  	-- Convert the type arguments to core.
			let tsInstC	= map toCoreT tsInst
			
			-- If the function being instantiated needs some context then there'll be a 
			--	separate witness for it... therefore we can safely erase contexts on
			--	type arguements for the instantiation.
			let tsInstCE	= map C.stripToShapeT tsInstC
			
			-- If we're in a substitution context do to a contra-variant port rewrite,
			--	then apply it to the arguments.
			mPortSub 	<- gets corePortSub
			let tsInstC_portSub	= 
				case mPortSub of
				 Nothing	-> tsInstCE
				 Just portSub	-> map (C.substituteT portSub) tsInstCE

			
			-- Work out what types belong to each quantified var in the type
			--	being instantiated.			
			let tsSub	= Map.fromList $ zip (map fst vtsForall) tsInstC_portSub

			-- If this function needs a witnesses we'll just make them up.
			--	Real witnesses will be threaded through in a later stage.
			let tsContextC'	= map (C.substituteT tsSub) tsContextC
			
			trace ("varInst: "
				% vT 				% "\n"
				% "    T[vT]           =\n" %> tScheme 		% "\n"
				% "    context         = " % tsContextC		% "\n"
				% "    tsInstCE        = " % tsInstCE		% "\n"
				% "    tsInstC_portSub = " % tsInstC_portSub	% "\n"
				% "    tsSub           = " % tsSub 		% "\n"
				% "    tsContestC'     = " % tsContextC' 	% "\n")
				$ return ()
			
		  	return	$ C.unflattenApps (C.XVar v : map C.XType (tsInstC_portSub ++ tsContextC'))

		 -- recursive use of a let-bound variable
		 -- 	pass the args on the type scheme back to ourselves.
		 T.InstanceLetRec vUse vBind (Just tScheme)
		  -> do
			let tScheme'	= toCoreT tScheme
			let tsQuant	= C.slurpForallsT tScheme'
		  	return $ C.unflattenApps
			 	(C.XVar v : [C.XType (C.TVar k v) | (v, C.TKind k) <- tsQuant])

	_ 
	 -> panic stage
	 	$ pretty
		$ "toCoreX: cannot convert expression to core.\n" 
		% "    exp = " %> (D.transformN (\a -> (Nothing :: Maybe ())) xx) % "\n"



-----------------------
-- Alt
--
toCoreA	:: Maybe Var
	-> D.Alt Annot -> CoreM C.Alt
		
toCoreA mObj alt
 = case alt of
	D.AAlt _ gs x
	 -> do	
	 	gs'	<- mapM (toCoreG mObj) gs
		x'	<- toCoreX x
		
		return	$ C.AAlt gs' x'
			

	

-----------------------
-- Guard
--
toCoreG :: Maybe Var
	-> D.Guard Annot
	-> CoreM C.Guard

toCoreG mObj gg
	| D.GCase _ w		<- gg
	, Just objV		<- mObj
	= do	w'		<- toCoreW w
	 	return		$ C.GExp w' (C.XVar objV)
		
	| D.GExp _ w x		<- gg
	= do	w'		<- toCoreW w
	 	x'		<- toCoreX x
		return		$ C.GExp w' x'
		

-----------------------
-- Pat
--
toCoreW :: D.Pat Annot
	-> CoreM C.Pat
	
toCoreW ww
 = case ww of
 	D.WConLabel _ v lvs
	 -> do 
	 	lvts	<- mapM toCoreA_LV lvs
	 	return	$ C.WCon v lvts
	 
	D.WConst _ c
	 -> return	$ C.WConst c
	 

toCoreA_LV (D.LIndex nn i, v)
 = do	t	<- getType v
	return	(C.LIndex i, v, t)

toCoreA_LV (D.LVar nn vField, v)
 = do	t	<- getType v
 	return	(C.LVar vField, v, t)

