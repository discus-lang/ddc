
-- | Conversion of desugared expressions to core.
--   During the conversion, we need to keep track of what effect and closure variables
--   are in scope so we can create the effect and closure annotations correctly.
--  
--   For example, with the following expression:
--  	 /\(e1 :> E1 + E2). \(x : T1) of e1 -> ...
--
--   The variable "e1" is the name we get back from the constraint solver, which names
--   the equivalence class for that effect. Because this variable is bound by a /\ we
--   cannot simply annotate the inner lambda expression with the (strengthened) effect
--   (E1 + E2), we have to include the whole constraint, including the variable e1.
--
--   On the other hand, if we had an expression like:
--      \(y : T2) of e2 ...
--
--   Provided e2 is not bound in an enclosing scope, we can just replace the variable
--   but the effect of the equivalence class in the type graph.
--
module DDC.Desugar.ToCore.Exp
	( toCoreS
	, toCoreX
	, toCoreA
	, toCoreG
	, toCoreW )
where
import DDC.Desugar.Pretty			()
import DDC.Desugar.ToCore.Base
import DDC.Desugar.ToCore.Lambda
import DDC.Desugar.ToCore.Literal
import DDC.Desugar.ToCore.VarInst
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Base.Literal
import DDC.Base.DataFormat
import DDC.Base.SourcePos
import DDC.Var
import Shared.VarPrim
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Util
import Shared.VarUtil				(isDummy, varPos)
import qualified DDC.Type			as T
import qualified DDC.Desugar.Transform		as D
import qualified DDC.Desugar.Exp		as D
import qualified DDC.Core.Exp			as C
import qualified Core.Util			as C
import qualified Data.Map			as Map
import qualified Data.Set			as Set
import qualified Debug.Trace

stage		= "DDC.Desugar.ToCore.Exp"
debug		= False
trace ss x	= if debug then Debug.Trace.trace (pprStrPlain ss) x else x

-- Stmt --------------------------------------------------------------------------------------------
-- | Convert a desugared statement to the core language.
toCoreS	:: Set Var		-- ^ Set of type variables bound in the environment.
	-> D.Stmt Annot		-- ^ The statement to convert.
	-> CoreM (Maybe C.Stmt)
		
toCoreS vsBound (D.SBind _ Nothing x)
 = do	x'		<- toCoreX vsBound x
	return $ Just $ C.SBind Nothing x'

toCoreS vsBound (D.SBind _ (Just v) x) 
 = do	-- lookup the generalised type of this binding.
	Just tScheme	<- lookupType v

	-- Calling fillLambdas below may add type abstractions around the
	-- expression. We need to know what the bound variables are when
	-- converting the expression to core. The bound vars are exactly
	-- the forall-bound vars in the type scheme for the binding.
	let (bksForall, _, _) 	= T.stripForallContextT tScheme
	let vsBoundHere		= Set.fromList $ catMaybes 
				$ map (T.takeVarOfBind . fst) bksForall
	
	let vsBound'		= Set.union vsBound vsBoundHere
	
 	-- convert the right to core.
	xCore	<- toCoreX vsBound' x

	-- add type abstractions.
	xLam	<- fillLambdas v tScheme xCore

	-- add a type annotation to the innermost body of the set of abstractions
	-- this makes it easy to determine the type of the whole binding later.
	let xLam_annot = C.dropXTau xLam Map.empty tScheme

	return $ Just $ C.SBind (Just v) xLam_annot

-- we don't need separate type sigs in the core language.
toCoreS	_ D.SSig{}
 = return Nothing

toCoreS _ ss
 	= panic stage 	
	$ "no match for " % show ss
	% " should have been eliminated by original source desugaring."


-- Exp ---------------------------------------------------------------------------------------------
-- | Expressions
toCoreX	:: Set Var		-- ^ Set of type variables bound in the environment.
	-> D.Exp Annot 		-- ^ The expression to convert.
	-> CoreM C.Exp

toCoreX vsBound xx
 = case xx of

	D.XLambdaTEC 
		_ v x 
		(T.TVar kV (T.UVar vTV)) 
		eff clo

	 | kV == T.kValue
	 -> do	
		-- Strip contexts off argument types, if we need the associated witnesses then these
		--	will be passed into the outer function.
		Just tArg1	<- lookupType vTV
		let  tArg	= T.stripToBodyT tArg1

		-- If the effect/closures were vars then look them up from the graph
		effAnnot	<- loadEffAnnot eff
		cloAnnot	<- loadCloAnnot clo
		x'		<- toCoreX vsBound x
		
		-- carry down set of quant type vars
		-- sink annot var. If it's in the quant set we have to add a more-than constraint.
		-- otherwise just add the effects.
		
		return	
		 $ trace (vcat 
			[ ppr "toCoreX: XLam"
			, "eff      = " % eff
			, "effAnnot = " % effAnnot
			, "clo      = " % clo	
			, "cloAnnot = " % cloAnnot ])
		 $ C.XLam v tArg x'
			(T.packT $ effAnnot)
			(T.packT $ cloAnnot)


	D.XApp	_ x1 x2
	 -> do
	 	x1'	<- toCoreX vsBound x1
		x2'	<- toCoreX vsBound x2
		return	$ C.XApp x1' x2'


	-- case match on a var
	D.XMatch _ (Just (D.XVar _ varX)) alts
	 -> do	alts'		<- mapM (toCoreA vsBound (Just (varX, T.TNil))) alts
		
		return	$ C.XDo	[ C.SBind Nothing (C.XMatch alts') ]

	-- case match on an exp
	D.XMatch _ (Just x) alts
	 -> do	x'	<- toCoreX vsBound x
		varX	<- newVarN NameValue
		
		alts'	<- mapM (toCoreA vsBound (Just (varX, T.TNil))) alts
		
		return	$ C.XDo	[ C.SBind (Just varX) x'
				, C.SBind Nothing (C.XMatch alts') ]
			
	-- regular  match
	D.XMatch _ Nothing alts
	 -> do	alts'	<- mapM (toCoreA vsBound Nothing) alts
		
		return	$ C.XDo	[ C.SBind Nothing (C.XMatch alts') ]
		
	-- primitive constants
	D.XLit (Just (T.TVar kV (T.UVar vT), _)) _
	 | kV	== T.kValue
	 -> do	
	 	Just t		<- lookupType vT
	 	let t_flat	= T.stripToBodyT $ T.flattenT t

		return	$ toCoreXLit t_flat xx

 
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
	 		$  mapM (toCoreS vsBound) stmts
	
		case takeLast stmts' of
		 Just stmt@(C.SBind (Just _) _)
		  -> panic stage
		  	$ "toCoreX: last statement of a do block cannot be a binding.\n"
			% "    offending statement:\n"	%> stmt	% "\n"
		 
		 _ -> return	$ C.XDo stmts'
			
			
	D.XIfThenElse _ e1 e2 e3
	 -> do
		v	<- newVarN NameValue

		e1'	<- toCoreX vsBound e1

		e2'	<- toCoreA vsBound
				(Just (v, T.TNil)) 
				(D.AAlt Nothing [D.GCase Nothing (D.WConLabel Nothing primTrue  [])] e2)

		e3'	<- toCoreA vsBound
				(Just (v, T.TNil)) 
				(D.AAlt Nothing [D.GCase Nothing (D.WConLabel Nothing primFalse [])] e3)
		
		return	$ C.XDo	[ C.SBind (Just v) e1'
				, C.SBind Nothing (C.XMatch [ e2', e3' ]) ]
	
	-- projections
	D.XProjTagged 
		(Just 	( T.TVar kV _
			, T.TVar kE _))
		vTagInst _ x2 _
	 | kV == T.kValue
	 , kE == T.kEffect
	 -> do	x2'		<- toCoreX vsBound x2
		
		-- lookup the var for the projection function to use
		projResolve	<- gets coreProjResolve

		let vProj
		 	= case Map.lookup vTagInst projResolve of
				Nothing	-> panic stage
					$ "No projection function for " % vTagInst % "\n"
					% " exp = " % xx % "\n\n"

				Just v	-> v
							
		x1'	<- toCoreVarInst vProj vTagInst
			
		return	$ C.XApp x1' x2'

	D.XProjTaggedT
		(Just 	( T.TVar kV _
			, T.TVar kE _))
		vTagInst _ _
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
		(Just (T.TVar kV (T.UVar vT), _))
		v
	 | kV == T.kValue
	 -> 	toCoreVarInst v vT

	_ 
	 -> panic stage
		$ "toCoreX: cannot convert expression to core.\n" 
		% "    exp = " %> (D.transformN (\_ -> (Nothing :: Maybe ())) xx) % "\n"


-- Alt ---------------------------------------------------------------------------------------------
-- | Convert a case alternative to core form.
toCoreA	:: Set Var			-- ^ Set of type variables bound in the environment.
	-> Maybe (Var, T.Type)		-- ^ Name and type of the discriminant.
	-> D.Alt Annot			-- ^ The alternative to convert.
	-> CoreM C.Alt
		
toCoreA vsBound mObj alt
 = case alt of
	D.AAlt _ gs x
	 -> do	gs'	<- mapM (toCoreG vsBound mObj) gs
		x'	<- toCoreX vsBound x
		
		return	$ C.AAlt gs' x'
	
	
-- Guards ------------------------------------------------------------------------------------------
-- | Convert a guard to core form.
toCoreG :: Set Var			-- ^ Set of type variables bound in the environment
	-> Maybe (Var, T.Type)		-- ^ Name and type of the discriminant.
	-> D.Guard Annot		-- ^ The guard to convert.
	-> CoreM C.Guard

toCoreG vsBound mObj gg
	| D.GCase _ w		<- gg
	, Just (vObj, tObj)	<- mObj
	= do	(w', mustUnbox)	<- toCoreW w

		let x			= C.XVar vObj tObj
		let Just tUnboxed	= takeUnboxedOfBoxedType tObj
		let Just ptUnboxed	= takePrimTypeOfType tUnboxed
		let Just tUnboxFn	= error ("need type of unboxing function")

		let xUnboxDiscrim
		  	=        (C.XPrim (C.MUnbox ptUnboxed) tUnboxFn) 
		  	`C.XAPP` tR
		  	`C.XApp` ((C.XPrim (C.MForce) tForceFn) `C.XApp` x)
					tR[C.XPrimType r, C.XPrim C.MForce [x]])
			= 
		case mustUnbox of
		 Just tR	
		  -> return 
		  $  C.GExp w' 


		 Nothing	-> return $ C.GExp w' x
		
	| D.GExp _ w x		<- gg
	= do	(w', mustUnbox)	<- toCoreW w
	 	x'		<- toCoreX vsBound x
		
		-- All literal matching in core is unboxed, so we must unbox the match object if need be.
		case mustUnbox of
		 Just r		-> return $ C.GExp w' (C.XPrim C.MUnbox [C.XPrimType r, C.XPrim C.MForce [x']])
		 Nothing	-> return $ C.GExp w' x'

 	| otherwise
	= panic stage 
		$ "no match for " % show gg
		% " should have been eliminated by original source desugaring."


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
	| D.WLit (Just	( T.TVar kV (T.UVar vT)
			, _))
			(LiteralFmt lit fmt) <- ww
	, kV == T.kValue

	, dataFormatIsBoxed fmt
	= do	mT	<- liftM (liftM T.stripToBodyT)
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
		(Just 	( T.TVar kV (T.UVar vT)
			, _ )) 
		litFmt@(LiteralFmt _ fmt)	<- ww
	, kV == T.kValue
	, dataFormatIsUnboxed fmt
	= do	return	( C.WLit (varPos vT) litFmt
			, Nothing)


	-- match against a variable
	| D.WVar (Just 	(T.TVar kV _
			, _))
		var		<- ww
	, kV == T.kValue
	= do
		return	( C.WVar var
			, Nothing)

	| otherwise
	= panic stage
		$ "tCoreW: no match for " % show ww % "\n"
	 

toCoreA_LV (D.LIndex _ i, v)
 = do	Just t		<- lookupType v
 	let t_flat	= (T.flattenT . T.stripToBodyT) t
	return	(C.LIndex i, v, t_flat)

toCoreA_LV (D.LVar _ vField, v)
 = do	Just t		<- lookupType v
 	let t_flat	= (T.flattenT . T.stripToBodyT) t
 	return	(C.LVar vField, v, t_flat)
