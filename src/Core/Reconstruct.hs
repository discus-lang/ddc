
-- | Reconstruct and check the type\/region\/effect\/closure and witness information in
--   the core IR.
--
--   The types of free variables can be supplied either as annotations on the variables themselves, 
--   or supplied via a table. Any missing annotations are filled during checking, so expressions
--   returned may be checked again without using the table.
--	
--   The table also carries the name of the calling function, to print in panic messages in case
--   a type error is uncovered.
--
--   The prime versions of the recon* functions start with a stage name and an empty table, but are 
--   otherwise identical to the plain versions.
--
--   The recon?_type versions take a stage name and only return the value type of the expression.
--
--   TODO: do proper checking of witnesses.
--   TODO: this needs cleaning up and auditing against the typing rules.
--   TODO: it would be better to store the header and module globs directly in the environment.
--         At the moment we're just slurping out all of the top level type and stashing them
--         in the initial environment, while using concat, which is nasty.
--
module Core.Reconstruct
	( reconTree, reconTreeWithEnv
	, reconP', reconP_type
	, reconX', reconX_type
	, reconBoxType
	, reconUnboxType)
where
import Core.Util
import Core.Plate.FreeVars
import Core.Reconstruct.Apply
import Core.Reconstruct.Environment
import Shared.VarPrim
import DDC.Base.DataFormat
import DDC.Base.Literal
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Core.Exp
import DDC.Core.Glob
import DDC.Type.Exp
import DDC.Type.Flatten
import DDC.Type.Kind
import DDC.Type.Compounds
import DDC.Type.Builtin
import DDC.Type.Trim
import DDC.Type.JoinSum
import DDC.Var.VarId		as Var
import DDC.Var
import DDC.Util.Doc
import Data.Traversable		(mapM)
import Type.Error		(Error(..))
import Type.Docable		()
import Prelude			hiding (mapM)
import Util			hiding (mapM)
import Type.Util
import qualified DDC.Var.PrimId	as Var
import qualified Data.Set	as Set
import qualified Debug.Trace	

stage		= "Core.Reconstruct"
debug		= False
trace ss x	= if debug then Debug.Trace.trace (pprStrPlain ss) x else x
		

-- Reconstruct Monad --------------------------------------------------------------------------
type ReconM
	= State Env

-- | Modify the state for an action, the revert to the original state.
tempState :: (Env -> Env) -> ReconM a -> ReconM a
tempState fSt action
 = do	st	<- get
        modify	fSt
	x	<- action
	put	st
	return	x

-- | Revert to the previous state after running an action.
keepState :: ReconM a -> ReconM a
keepState = tempState id


-- Tree --------------------------------------------------------------------------------------------
-- | Do type reconstruction on this tree.
reconTree
	:: String	-- ^ Caller name to be printed in panic messages.
	-> Glob		-- ^ Header glob.
	-> Glob 	-- ^ Module glob.
	-> Glob		-- ^ Module with reconstructed and checked type information.
	
reconTree caller cgHeader cgModule
 = reconTreeWithEnv
 	(initEnv (Just caller) cgHeader cgModule)
	cgHeader cgModule


-- | Do type reconstruction on this tree, with the given startning environment.
reconTreeWithEnv
	:: Env	
	-> Glob		-- ^ Header glob.
	-> Glob 	-- ^ Module glob.
	-> Glob		-- ^ Module glob with reconstructed type information.
	
reconTreeWithEnv env cgHeader cgCore
 = {-# SCC "reconstructTree" #-}
   let	
	-- reconstruct type info on each top level thing.
	--	We have to do the regions first so their witnesses are added to the state.
	(pRegions', pBinds')		
	 = evalState 
		(do pRegions'	<- mapM reconP $ globRegion cgCore
		    pBinds'	<- mapM reconP $ globBind   cgCore
		    return (pRegions', pBinds'))
		env

	cgCore'	= cgCore 
		{ globRegion	= pRegions'
		, globBind	= pBinds'}
	
   in	cgCore'


-- Top ---------------------------------------------------------------------------------------------
reconP' :: String -> Top -> (Top, Type, Effect, Closure)
reconP' caller (PBind v x)
 = let	env	= initEnv (Just caller) globEmpty globEmpty

	(SBind (Just v') x', typ', eff', clo')
		= evalState
			(reconS $ SBind (Just v) x)
			env
		
   in  (PBind v' x', typ', eff', clo')
 

reconP_type :: String -> Top -> Type
reconP_type caller p
	= t4_2 $ reconP' caller p


reconP	:: Top -> ReconM Top
reconP (PBind v x)
 = do	tt			<- get
	(x', xT, xE, xC)	<- {-# SCC "reconP/reconX" #-} reconX x
	let	xE'		= {-# SCC "reconP/maskE" #-} maskE xT xE xC
		xE''		= simplifyE $ maskConstRead xE'

		maskConstRead :: Effect -> Effect
		maskConstRead	e@(TSum _ [])	= e
		maskConstRead	e@(TApp t1 tR)
			| t1 == tRead
			, isConst tR
			= tPure
		maskConstRead	(TSum k es)	= TSum k (map maskConstRead es)
		maskConstRead	e		= e

		isConst (TVar k (UVar r))
		 = (k == kRegion) && (isJust $ lookupWitnessConst r tt)

		simplifyE e@(TSum _ []) = e
		simplifyE (TSum k es)
		 | null es'		= tPure
		 | null (drop 1 es')	= head es'
		 | otherwise		= TSum k es'
			where es' = filter (/= tPure) $ map simplifyE es
		simplifyE e = e

	trace	("reconP[PBind]: (/\\ " % v % " -> ...)\n"
		% "  x':\n"  %> x'	% "\n"
		% "  xT:\n"  %> xT	% "\n"
		% "  xE:\n"  %> xE	% "\n"
		% "  xE':\n" %> xE'	% "\n"
		% "  xE'':\n" %> xE''	% "\n"
		% "  xC:\n"  %> xC	% "\n\n") $ return ()

        unless (xE'' == tPure)
		$ dieWithUserError [ErrorEffectfulCAF (v, xT) xE'']

	return	(PBind v x')

reconP p@(PRegion v wits)
 = do	trace ("reconP[PRegion]: " % v % ": " % wits) $ return ()
	let	constWits	= foldr f [] wits
		f (w,k) ws	= if isConst k then w : ws else ws

		isConst (TApp (TCon (TyConWitness TyConWitnessMkConst _)) _)
				= True
		isConst _	= False

	mapM_ (\w -> modify (addWitnessConst v w)) (take 1 constWits)
	return p

reconP p	= return p


-- Expression --------------------------------------------------------------------------------------
reconX' :: String -> Exp -> (Exp, Type, Effect, Closure)
reconX' caller x 
 = let	env	= initEnv (Just caller) globEmpty globEmpty
   in	evalState (reconX x) env

reconX_type :: String -> Exp -> Type
reconX_type caller x
	= t4_2 $ reconX' caller x

reconX :: Exp -> ReconM (Exp, Type, Effect, Closure)

-- LAM
reconX xx@(XLAM b@(BMore v t1) t2 x)
 = do	(x', xT, xE, xC)	<- tempState (addMoreVT v t1) $ reconX x
	return	( XLAM b t2 x'
		, TForall b t2 xT
		, xE
		, xC)

reconX (XLAM v k@KApp{} x)
 = do	(x', xT, xE, xC)	<- reconX x
	return	( XLAM 	   v k x'
		, TForall BNil k xT
		, xE
		, xC)

reconX (XLAM v k x)
 = do	(x', xT, xE, xC)	<- reconX x
   	return	( XLAM 	  v k x'
	    	, TForall v k xT 
		, xE
		, xC)
 
-- handle applications to string literals directly
--	this way we don't need to invent a variable for the forall bound region 
--	in the literal's type scheme. String literals never appear without their region
--	parameters in the core so this is ok.

reconX exp@(XAPP (XLit (LiteralFmt lit fmt)) (TVar k r))
 | k == kRegion
 = do	let	t = case lit of
		     LString{}
		      -> makeTData
				(primTString Unboxed)
				(KFun kRegion kValue)
				[TVar kRegion r]
		     _
		      -> panic stage
		      $  "reconX/XApp: non string constant applied to region\n"
		      %  "    exp = " % exp	% "\n"
	return	( exp
	   	, t
		, tPure
		, tEmpty)


reconX exp@(XAPP x t)
 = do	tt			<- get
	(x', tX, eX, cX)	<- reconX x
	let aT			=  applyTypeT tt tX t
	case aT of
	 Just tApp
	  ->	return	( XAPP x' t
			, tApp
			, eX
			, cX)
	  
	 _ -> panic stage
	 	$ " reconX: Kind error in type application (x t).\n"
		% "     caller = "	% getEnvCaller tt	% "\n"
		% "     x      =\n"	%> x			% "\n\n"
		% "-- type ------\n" 	
		%	(pprDocIndentedWithNewLines 0 $ doc t)	% "\n\n"
		% "   T[x]     =\n"	%> tX	% "\n\n"

-- xtau
-- We can't actually check the reconstructed type against the annotation here
--	because we can't see /\ bound TREC variables that might be bound above us.
--
--	eg, with /\ t -> [** t] x
--	    the type of x is (forall t. t), not just t.
--
-- 	The XTau types are checked by reconS instead.
--
reconX exp@(XTau tauT x)
 = do	(x', xT, xE, xC)	<- keepState $ reconX x
	return	( XTau tauT x'
		, xT
		, xE
		, xC)


-- lam
reconX exp@(XLam v t x eff clo)
 = {-# SCC "reconX/XLam" #-}
   do	tt			<- get
	(x', xT, xE, xC)	<- tempState (addEqVT v t) $ reconX x

	let	eff'		= packT $ substituteT ((flip lookupEqVT) tt) eff
		clo_sub		= packT $ substituteT ((flip lookupEqVT) tt) clo
		xE'		= {-# SCC "reconX/maskE" #-} maskE xT xE xC

	-- check effects match
	() <- unless (subsumes ((flip lookupMoreVT) tt) eff' xE') $
		panic stage
		$ "reconX: Effect error in core.\n"
		% "    caller = " 			% getEnvCaller tt	% "\n"
		% "    in lambda abstraction:\n" 	%> exp	% "\n\n"
		% "    reconstructed effect of body:\n" %> xE'	% "\n\n"
		% "    resulting from masking:\n" 	%> xE	% "\n\n"
		% "    is not <: annot on lambda:\n"	%> eff'	% "\n\n"
		% "    with bounds:\n"
		% "    t:       " %> t	 % "\n"

	-- check closures match
	-- Closures in core don't work properly yet.
	let	xC_masked	= dropTFreesIn (Set.singleton v) xC

		xC_flat		= flattenT_constrainForm 
				$ toConstrainFormT xC_masked

		xC'		= toFetterFormT 
				$ trimClosureC_constrainForm
				$ xC_flat

{-
	() <- unless (subsumes (envMore tt) clo_sub xC') $
		panic stage
		$ "reconX: Closure error in core.\n"
		% "    caller = " % envCaller tt	% "\n"
		% "    in lambda abstraction:\n" 	%> exp		% "\n\n"
		% "    reconstructed closure of body:\n"%> xC'		% "\n\n"
		% "    is not <: annot on lambda:\n"	%> clo_sub	% "\n\n"
		% pprBounds (envMore tt)
-}

	-- Now that we know that the reconstructed effect closures of the body is less
	--	than the annotations on the lambda we can reduce the annotation so it only
	--	contains members of the effect/closure that was reconstructed.
	--
	--	This is sound because we're only ever making the annotation smaller.
	--	If we reduced the annotation too far, ie so it wasn't <= than the reconstructed
	--	effect\/closure then we would get an error next type we called reconstruct.
	--
	--	We actually /have/ to do this clamping because when Core.Bind introduces local
	--	regions for mutually recursive functions, the left over effect annotations contain
	--	regions which are out of scope which makes Core.Lint complain.
	--
	--	Another way of looking at this is that we're doing the effect masking that Core.Bind
	--	should have done originally.
	--
	eff_clamped	<- clampSum xE' eff'

	-- don't clamp closures. There is no need to, and clampSum gives the wrong answer
	--	because two closures   (x : Int %r1) and (y : Int %r1) are taken to be non-equal
	--	due to their differing tags.
--	clo_clamped	<- clampSum xC' clo_sub

	return	( XLam v t x' eff_clamped clo_sub
		, makeTFun t xT eff_clamped clo_sub
		, tPure
		, xC')

-- local
-- TODO: check well foundedness of witnesses
-- 	Note that for performance reasons, we don't mask effects local effects here.
--	All the local effects from the body of a lambda abstraction are masked
--	all at once in the rule for XLam.
--
-- not sure if we should worry about regions variables escaping here.
-- we're not doing stack allocation, so it doesn't matter if they do.

reconX (XLocal v vs x)
 = do	(x', xT, xE, xC)	<- keepState $ reconX x
	return	( XLocal v vs x'
	   	, xT
		, xE
		, xC )
	
-- app
reconX exp@(XApp x1 x2)
 = do	tt			<- get
	(x1', x1t, x1e, x1c)	<- keepState $ reconX x1
	(x2', x2t, x2e, x2c)	<- keepState $ reconX x2
	let mResultTE		=  applyValueT tt x1t x2t
	case mResultTE of
	 Just (appT, appE)
	  -> let x'		= XApp x1' x2'
		 xE		= makeTSum kEffect  [x1e, x2e, appE]
		 xC		= makeTSum kClosure [x1c, x2c]
	     in	return (x', appT, xE, xC)
	       	
	 _ -> panic stage	
	 	$ "reconX: Type error in value application (x1 x2).\n"
		% "    x1:\n" %> x1	% "\n\n"
		% "    x2:\n" %> x2	% "\n\n"

		% "   T[x1]   = " %> x1t		% "\n\n"
		% "   (flat)  = " %> (flattenT_constrainForm $ toConstrainFormT x1t) % "\n\n"

		% "   T[x2]   = " % x2t		% "\n\n"

-- do
reconX (XDo ss)
 = do	tt	<- get
	(ss', sTs, sEs, sCs)	<- fmap unzip4 $ keepState $ mapM reconS ss
	let	Just t		= takeLast sTs
		vsBind		= catMaybes $ map takeVarOfStmt ss'

	return	( XDo ss'
        	, t
		, makeTSum kEffect sEs
		, makeTSum kClosure
			$ filter (\c -> case takeTFree c of
					  Just (v, _)	-> notElem v vsBind
					  _		-> True)
			$ flattenTSum
			$ makeTSum kClosure sCs)
   
-- match
reconX (XMatch [])
 = panic stage
 	$ "reconX: XMatch has no alternatives\n"

reconX (XMatch aa)
 = do	(aa', altTs, altEs, altCs)
		<- fmap unzip4 $ mapM reconA aa

	-- join all the alt types to get the type for the whole match
	--	expression.
	let	Just tMatch	= joinSumTs altTs

	return	( XMatch aa'
		, tMatch
		, makeTSum kEffect altEs
		, makeTSum kClosure altCs)

-- TODO: check against existing annotation.
-- var has no type annotation, so look it up from the table
reconX (XVar v TNil)
 = do	tt		<- get
	let	tM	= lookupEqVT v tt
	when (isNothing tM)
	 $ panic stage
		$ "reconX: Variable " % v % " has no embedded type annotation and "
		% "is not in the provided environment.\n"
		% "    caller = " % getEnvCaller tt	% "\n"

	let	Just t	= tM
		t'	= toFetterFormT
			$ flattenT_constrainForm 
			$ substituteT ((flip lookupEqVT) tt)
			$ toConstrainFormT t

		-- When we add the type to this var we need to attach any more constraints associated with it,
		--	else we won't be able to check expressions separate from their enclosing XLAMs
		--	(which carry these constraints)
		vsFree	= freeVars t
		vtsMore	= catMaybes
			$ map (\u -> fmap ((,) u) $ lookupMoreVT u tt)
			$ Set.toList vsFree

		tDrop	= makeTFetters t'
				[ FMore (TVar k $ UVar v) t2
					| (v, t2)	<- vtsMore
					, let Just k	= defaultKindOfVar v]

	return	( XVar v tDrop
		, tDrop
		, tPure
		, makeTFreeBot v t)

-- var has a type annotation, so use that as its type
reconX (XVar v t)
 = do	tt	<- get

	let t'	= toFetterFormT
		$ flattenT_constrainForm 
		$ substituteT ((flip lookupEqVT) tt)
		$ toConstrainFormT t

	return	( XVar v t
		, t'
		, tPure
		, toFetterFormT
			$ trimClosureC_constrainForm
			$ toConstrainFormT 
			$ makeTFreeBot v t)


-- prim
--	BUGS: 	effects from primitive applications are not generated
--		this isn't a problem at the momement because we don't check effect information
--		after Core.Curry
--
reconX xx@(XPrim prim xs)
 = do	trace ("reconX[XPrim]: " % xx % "\n") $ return ()
	tt	<- get
	-- some of the xs are type terms which we can't call recon on
	--	so we have to do some contortions to get the closure from the others.
	let	reconMaybeX x
		 = case x of
		    XPrimType{}
		     ->	return (x, Nothing, Nothing, Nothing)
		    _
		     ->	do	(x', typ, eff, clo)	<- keepState $ reconX x
				return (x', Just typ, Just eff, Just clo)
		 
	(xs', txs, xsmEs, xsmCs)
		<- fmap unzip4 $ mapM reconMaybeX xs

	-- work out the result type and effect of applying the primitive operator
	let getPrimTE

		-- boxing		
		| MBox 		<- prim
		, [XPrimType r, x]	<- xs
		= do	rx	<- reconX x
			return	( reconBoxType r $ t4_2 $ rx
				, tPure)
		
		-- unboxing
		| MUnbox	<- prim
		, [XPrimType r, x]	<- xs
		= do	rx	<- reconX x
			return	( reconUnboxType r $ t4_2 $ rx
				, TApp tRead r)
		  
		-- forcing
		| MForce	<- prim
		, [Just t1]	<- txs
		= do	return	( t1
				, tPure)	
		
		| MCall PrimCallTail{}	<- prim
		= do	rxs'	<- reconApps xs'
			return	( rxs'
				, tPure)

		| MCall PrimCallSuper{}	<- prim
		= do	rxs'	<-reconApps xs'
		  	return	( rxs'
				, tPure)

		| MCall PrimCallSuperApply{}	<- prim
		= do	rxs'	<- reconApps xs'
			return	( rxs'
				, tPure)

		| MCall PrimCallApply{}	<- prim
		= do	rxs'	<- reconApps xs'
			return	( rxs'
				, tPure)

		| MCall PrimCallCurry{}	<- prim
		= do	rxs'	<- reconApps xs'
			return	( rxs'
				, tPure)

		| MOp op	<- prim
		= reconOpApp op xs'

		| otherwise
		= panic stage
		$ "reconX/Prim: no match for " % prim <> punc " " xs % "\n"

	(tPrim, ePrim)	<- getPrimTE
		
	return	( XPrim prim xs'
		, tPrim
		, makeTSum kEffect  $ ePrim : catMaybes xsmEs
		, makeTSum kClosure $ catMaybes xsmCs)


reconX xx@(XLit litFmt)
 = do	let	Just tcLit	= tyConOfLiteralFmt litFmt
		tLit		= TCon tcLit
	return	( xx
		, tLit
		, tPure
		, tEmpty)


-- no match
reconX xx
 = do	tt	<- get
	panic stage 
	 	$ "reconX: no match for " % show xx	% "\n"
		% "    caller = " % getEnvCaller tt	% "\n"


-- | Convert this boxed type to the unboxed version
reconBoxType :: Region -> Type -> Type
reconBoxType r tt
	| Just (v, k, _)		<- takeTData tt
	, (baseName, mkBind, fmt)	<- splitLiteralVarBind (varId v)
	, Just fmtBoxed			<- dataFormatBoxedOfUnboxed fmt
	= makeTData 
		(primVarFmt NameType 
				(pprStrPlain (varModuleId v) ++ "." ++ baseName) 
				mkBind fmtBoxed)
		(KFun kRegion kValue) 
		[r]


-- | Convert this type to the unboxed version
reconUnboxType :: Region -> Type -> Type
reconUnboxType r1 tt
	| Just (v, k, [r2@(TVar kV _)])	<- takeTData tt
	, kV == kRegion
	, r1 == r2
	, (baseName, mkBind, fmt)	<- splitLiteralVarBind (varId v)
	, Just fmtUnboxed		<- dataFormatUnboxedOfBoxed fmt
	= makeTData
		(primVarFmt NameType 
				(pprStrPlain (varModuleId v) ++ "." ++ baseName)
				mkBind fmtUnboxed)
		kValue
		[]


-- | Split the VarBind for a literal into its components,
--	eg  TInt fmt -> ("Int", TInt, fmt)
splitLiteralVarBind
	:: Var.VarId
	-> ( String
	   , DataFormat -> Var.PrimId
	   , DataFormat)

splitLiteralVarBind (VarIdPrim pid)
 = case pid of
 	Var.TBool  fmt	-> ("Bool",   Var.TBool,   fmt)
	Var.TWord  fmt	-> ("Word",   Var.TWord,   fmt)
	Var.TInt   fmt	-> ("Int",    Var.TInt,    fmt)
	Var.TFloat fmt	-> ("Float",  Var.TFloat,  fmt)
	Var.TChar  fmt	-> ("Char",   Var.TChar,   fmt)
	Var.TString fmt	-> ("String", Var.TString, fmt)



-- | Reconstruct the type and effect of an operator application.
--	Not sure if doing this manually is really a good way to do it.
--	It'd be nice to have a more general mechanism like GHC rewrite rules..
reconOpApp :: PrimOp -> [Exp] -> ReconM (Type, Effect)
reconOpApp op xs
 = do	rxs <- mapM (fmap t4_2 . reconX) xs

	let result
		-- arithmetic operators
		| op == OpNeg
		, [t1]		<- rxs
		, isUnboxedNumericType t1
		= (t1, tPure)

		| elem op [OpAdd, OpSub, OpMul, OpDiv, OpMod]
		, [t1, t2]	<- rxs
		, isUnboxedNumericType t1
		, t1 == t2
		= (t1, tPure)

		-- comparison operators
		| elem op [OpEq, OpNeq, OpGt, OpGe, OpLt, OpLe]
		, [t1, t2]	<- rxs
		, isUnboxedNumericType t1
		, t1 == t2
		= (makeTData (primTBool Unboxed) kValue [], tPure)

		-- boolean operators
		| elem op [OpAnd, OpOr]
		, [t1, t2]		<- rxs
		, Just (v, k, [])	<- takeTData t1
		, v == (primTBool Unboxed)
		, t1 == t2
		= (makeTData (primTBool Unboxed) kValue [], tPure)

		| otherwise
		= panic stage
		$ "reconOpApp: no match for " % op % " " % xs % "\n"

	return result


-- | Check whether a type is an unboxed numeric type
isUnboxedNumericType :: Type -> Bool
isUnboxedNumericType tt
 	| Just (v, _, []) <- takeTData tt
	, isUnboxedNumericType_varId (varId v)
	= True

	-- treat pointers as numeric types
	| Just (v, _, _) <- takeTData tt
	, v == primTPtrU
	= True
	
	| otherwise
	= False

isUnboxedNumericType_varId vid
 = case vid of
 	Var.VarIdPrim (Var.TWord _)		-> True
	Var.VarIdPrim (Var.TInt _)		-> True
	Var.VarIdPrim (Var.TFloat _)		-> True
	_					-> False



-- | Reconstruct the result type when this list of expressions
--	is applied in a left to right order.
--
--	eg  [x1, x2, x3, x4] =>  ((x1 x2) x3) x4
--
reconApps 
	:: [Exp] 
	-> ReconM Type

reconApps [XPrimType t]
 =	return t
 
reconApps [x]
 = 	fmap t4_2 $ reconX x

reconApps (XPrimType t1 : XPrimType t2 : xs)
 = do	table		<- get
	let Just t12	=  applyTypeT table t1 t2
	reconApps (XPrimType t12 : xs)
 	
reconApps (x1 : XPrimType t2 : xs)
 = do	table		<- get
	t1		<- fmap t4_2 $ keepState $ reconX x1
	let Just t12	= applyTypeT table t1 t2
	reconApps (XPrimType t12 : xs)

reconApps (XPrimType t1 : x2 : xs)
 = do	table		<- get
	t2		<- fmap t4_2 $ keepState $ reconX x2
 	let Just (t12, _) = applyValueT table t1 t2
	reconApps (XPrimType t12 : xs)

reconApps (x1 : x2 : xs)
 = do	table		<- get
	t1		<- fmap t4_2 $ keepState $ reconX x1
	t2		<- fmap t4_2 $ keepState $ reconX x2
	let Just (t12, _) = applyValueT table t1 t2
	reconApps (XPrimType t12 : xs)


-- Stmt --------------------------------------------------------------------------------------------

-- | running reconS also adds a type for this binding into the table
reconS	:: Stmt -> ReconM (Stmt, Type, Effect, Closure)
reconS (SBind Nothing x)	
 = do	tt			<- get
	(x', xT, xE, xC)	<- keepState $ reconX x

	return	( SBind Nothing x'
		, xT
		, xE
		, xC)

reconS (SBind (Just v) x)
 = do	tt	<- get
	(x', xT, xE, xC)	<- reconX x
	put	$ addEqVT v xT tt

	return	( SBind (Just v) x'
		, xT
		, xE
		, dropTFreesIn
			(Set.singleton v)
			xC)
	  

-- Alt ---------------------------------------------------------------------------------------------
reconA 	:: Alt -> ReconM (Alt, Type, Effect, Closure)
reconA (AAlt gs x)
 = do	tt				<- get
	gecs				<- mapM reconG gs
	let (gs', vssBind, gEs, gCs)	= unzip4 gecs
	(x', xT, xE, xC)		<- reconX x
	put tt
	return	( AAlt gs' x'
   		, xT
		, makeTSum kEffect (gEs ++ [xE])
		, dropTFreesIn
			(Set.fromList $ concat vssBind)
			$ makeTSum kClosure (xC : gCs))

   
-- Guards ------------------------------------------------------------------------------------------

-- | running reconG also adds types for the matched variables into the table.

-- TODO: check type of pattern against type of expression
reconG	:: Guard -> ReconM (Guard, [Var], Effect, Closure)
reconG gg@(GExp p x)
 = do	tt		<- get
	(x', tX, eX, cX)<- reconX x
	let	binds	= slurpVarTypesW tX p

		-- Work out the effect of testing the case object.
		tX_shape	= stripToBodyT tX

		effTest
			-- If the LHS of the guard is just a var then there is no 'match' per se, and no effects.
			| WVar{}		<- p
			= tEmpty

			-- If the type of the object has no regions we assume that
			--	it is constant, so matching against it generates no effects.
			| Just (vD, _, [])	<- takeTData tX_shape
			= tEmpty

			-- matching against some object cause a read effect on its primary region.
			| Just (vD, _, TVar k rH : _) <- takeTData tX_shape
			, k == kRegion
			= TApp tRead (TVar kRegion rH)

			-- object does not have a primary region, assume it is constant
			| otherwise
			= tEmpty

			| otherwise
			= panic stage
			$ "reconG: no match for:\n"
			% "  p        = " % p % "\n"
			% "  tX_shape = " % tX_shape	% "\n"

	put	$ foldr (uncurry addEqVT) tt binds

	return	( GExp p x'
		, map fst binds
	   	, makeTSum kEffect ([eX, effTest])
		, cX)

slurpVarTypesW tRHS (WVar v)		= [(v, tRHS)]
slurpVarTypesW tRHS (WLit{})		= []
slurpVarTypesW tRHS (WCon _ v lvt)	= map (\(l, v, t)	-> (v, t)) lvt

	
-- Mask effects ------------------------------------------------------------------------------------
-- | Mask non-observable effects that are not in the
--	environment (closure) or type of the return value.
maskE	:: Type -> Effect -> Closure -> Effect
maskE xT xE xC
 = let	fvT		= freeVars xT
	fvC		= freeVars xC

	xE_masked	= maskReadWriteNotIn (Set.union fvT fvC) 
			$ packT xE

   in	xE_masked


-- Clamp -------------------------------------------------------------------------------------------
-- | Clamp a sum by throwing out any elements of the second one that are not members of the first.
--	Result is at least as big as t1.
clampSum :: Type -> Type -> ReconM Type
clampSum t1 t2
	| kindOfType t1 == kindOfType t2
	= do	table	<- get
		let	parts2		= flattenTSum t1
			parts_clamped	= [p	| p <- parts2
						, subsumes ((flip lookupMoreVT) table) t1 p]
			k1		= kindOfType t1

		return $ makeTSum k1 parts_clamped

