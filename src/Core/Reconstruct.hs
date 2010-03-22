
-- |	Reconstruct and check the type\/region\/effect\/closure and witness information in
--	the core IR.
--
-- 	The types of free variables can be supplied either as annotations on the variables themselves, 
--	or supplied via a table. Any missing annotations are filled during checking, so expressions
--	returned may be checked again without using the table.
--	
--	The table also carries the name of the calling function, to print in panic messages in case
--	a type error is uncovered.
--
--	The prime versions of the recon* functions start with a stage name and an empty table, but are 
--	otherwise identical to the plain versions.
--
--	The recon?_type versions take a stage name and only return the value type of the expression.
--	
-- 	TODO: also check witnesses and proofs of purity.
--
--
module Core.Reconstruct
	( reconTree, reconTree'
	, reconP, reconP', reconP_type
	, reconX, reconX', reconX_type
	, reconS
	, reconA
	, reconG

	, reconBoxType
	, reconUnboxType)
where
import Core.Exp
import Core.Util
import Core.Plate.FreeVars
import Type.Exp
import Type.Util.Environment
import Type.Builtin
import Shared.VarPrim
import Shared.Literal
import Util
import DDC.Var.VarId
import DDC.Var.NameSpace
import DDC.Base.DataFormat
import DDC.Main.Pretty
import DDC.Main.Error
import Type.Error		(Error(..))
import Type.Util		hiding (flattenT, trimClosureC_constrainForm)
import Shared.Var		(Var)
import qualified DDC.Var.PrimId	as Var
import qualified Shared.Var	as Var
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import qualified Debug.Trace	


-----
stage	= "Core.Reconstruct"

debug	= False
trace ss x	
	= if debug 
		then Debug.Trace.trace (pprStrPlain ss) x 
		else x
		

-- Reconstruct Monad --------------------------------------------------------------------------
type ReconM
	= State Env

-- | Modify the state for an action, the revert to the original state.
tempState
	:: (Env -> Env) -> ReconM a -> ReconM a
tempState fSt action
 = do	st	<- get
        modify	fSt
	x	<- action
	put	st
	return	x

-- | Revert to the previous state after running an action.
keepState
	:: ReconM a -> ReconM a
keepState = tempState id


-- Tree --------------------------------------------------------------------------------------------
reconTree'
	:: String	-- ^ caller name
	-> Tree		-- ^ header tree
	-> Tree 	-- ^ core tree
	-> Tree		-- ^ core tree with reconstructed type information
reconTree' caller tHeader tCore
 = reconTree
 	emptyEnv { envCaller = Just caller }
	tHeader tCore

reconTree
	:: Env	
	-> Tree		-- ^ header tree
	-> Tree 	-- ^ core tree
	-> Tree		-- ^ core tree with reconstructed type information
	
reconTree table tHeader tCore
 = {-# SCC "reconstructTree" #-}
   let	-- slurp out all the stuff defined at top level
	topTypes	= {-# SCC "reconTree/topTypes" #-} catMap slurpTypesP (tHeader ++ tCore)
 	tt		= {-# SCC "reconTree/table"    #-} foldr (uncurry addEqVT) table topTypes
	
	-- reconstruct type info on each top level thing
	tCore'		= evalState (mapM reconP tCore) tt
	
   in tCore'


-- Top ---------------------------------------------------------------------------------------------
reconP' :: String -> Top -> (Top, Type, Effect, Closure)
reconP' caller (PBind v x)
 = let (SBind (Just v') x', typ', eff', clo')
		= evalState	(reconS $ SBind (Just v) x)
				(emptyEnv { envCaller = Just caller })
		
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
		maskConstRead	e@(TBot _)	= e
		maskConstRead	e@(TEffect v rs)
		 | v == primRead
		 , all isConst rs
		 = tPure
		maskConstRead	(TSum k es)	= TSum k (map maskConstRead es)
		maskConstRead	e		= e

		isConst (TVar k r)
		 = (k == kRegion) && (isJust . Map.lookup r $ envWitnessConst tt)

		simplifyE e@(TBot _) = e
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

		isConst (TApp (TCon (TyConClass TyClassConst _)) _)
				= True
		isConst _	= False

	mapM_ (\w -> modify (addWitnessConst v w)) (take 1 constWits)
	return p

reconP p	= return p


-- Expression --------------------------------------------------------------------------------------
reconX' :: String -> Exp -> (Exp, Type, Effect, Closure)
reconX' caller x 
	= evalState (reconX x) emptyEnv { envCaller = Just caller }

reconX_type :: String -> Exp -> Type
reconX_type caller x
	= t4_2 $ reconX' caller x

reconX :: Exp -> ReconM (Exp, Type, Effect, Closure)

-- LAM
reconX xx@(XLAM b@(BMore v t1) t2 x)
 = do	(x', xT, xE, xC)	<- tempState (addMoreVT v t1) $ reconX x
	trace	("reconX[XLAM-BMore]: (/\\ " % b % " -> ...)\n"
		% "  xT:\n" %> xT	% "\n"
		% "  xC:\n" %> xC	% "\n\n") $ return ()

	return	( XLAM b t2 x'
		, TForall b t2 xT
		, xE
		, xC)

reconX (XLAM v k@KClass{} x)
 = do	(x', xT, xE, xC)	<- reconX x
	trace 	("reconX[XLAM-KClass]: (/\\ " % v % " -> ...)\n"
   		% "  xT:\n" %> xT	% "\n"
		% "  xC:\n" %> xC	% "\n\n") $ return ()
	return	( XLAM 	   v k x'
		, TContext k xT
		, xE
		, xC)

reconX (XLAM v k x)
 = do	(x', xT, xE, xC)	<- reconX x
	trace	("reconX[XLAM-any]: (/\\ " % v % " -> ...)\n"
   		% "  xT:\n" %> xT	% "\n"
		% "  xC:\n" %> xC	% "\n\n") $ return ()

   	return	( XLAM 	  v k x'
	    	, TForall v k xT 
		, xE
		, xC)
 
-- APP

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
	aT			<- applyTypeT tX t
	case aT of
	 Just tApp
	  ->	trace 	("reconX[XAPP]: (" % x % " @ " % t % ")\n"
			% "    tApp:\n"	%> tApp		% "\n"
			% "    eX:\n" 	%> eX		% "\n"
			% "    cX:\n"	%> cX		% "\n\n\n") $

		return	( XAPP x' t
			, tApp
			, eX
			, cX)
	  
	 _ -> panic stage
	 	$ " reconX: Kind error in type application (x t).\n"
		% "     caller = "  % envCaller tt	% "\n"
		% "     x      =\n" %> x		% "\n\n"
		% "     t      =\n" %> t		% "\n\n"
		% "   T[x]     =\n" %> tX	% "\n\n"

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
	(x', xT, xE, xC)	<- tempState	(addEqVT v t) $ reconX x

	let	eff'		= packT $ substituteT (envEq tt) eff
		clo_sub		= packT $ substituteT (envEq tt) clo
		xE'		= {-# SCC "reconX/maskE" #-} maskE	xT xE xC

	-- check effects match
	() <- unless (subsumes (envMore tt) eff' xE') $
		panic stage
		$ "reconX: Effect error in core.\n"
		% "    caller = " % envCaller tt	% "\n"
		% "    in lambda abstraction:\n" 	%> exp	% "\n\n"
		% "    reconstructed effect of body:\n" %> xE'	% "\n\n"
		% "    is not <: annot on lambda:\n"	%> eff'	% "\n\n"
		% "    with bounds:\n"
		% "    t:       " %> t	 % "\n"
		% "    env:     " %> xC  % "\n"
		% pprBounds (envMore tt)

	-- check closures match
	-- Closures in core don't work properly yet.
	let	xC_masked	= dropTFreesIn (Set.singleton v) xC
		xC_flat		= flattenT xC_masked
		xC'		= trimClosureC Set.empty Set.empty $ xC_flat

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

	trace ( "reconX[XLam]:\n"
		% "    xT           = " % xT	% "\n"
		% "    xE'  (recon) = " % xE'	% "\n"
		% "    eff' (annot) = " % eff'	% "\n"
		% "    eff_clamped  = " % eff_clamped	% "\n"
		% "    xE           = " % xE		% "\n"
	   	% "    clo          = " % xC		% "\n") $ return ()

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
reconX (XLocal v vs x)
 = do	(x', xT, xE, xC)	<- keepState $ reconX x

	-- not sure if we should worry about regions variables escaping here.
	-- we're not doing stack allocation, so it doesn't matter if they do.
	{-
	when (Set.member v (freeVars xT)) $
	  do tt	<- get
	     panic stage
		$ "reconX: region " % v % " is not local\n"
		% "    caller = " % envCaller tt	% "\n"
		% "    t      = " % xT	% "\n"
		% "    x      = " % x'	% "\n\n"
	-}
	return	( XLocal v vs x'
	   	, xT
		, xE
		, xC )
	
-- app
reconX exp@(XApp x1 x2 eff)
 = do	tt			<- get
	(x1', x1t, x1e, x1c)	<- keepState $ reconX x1
	(x2', x2t, x2e, x2c)	<- keepState $ reconX x2
	mResultTE		<- {-# SCC "reconX/applyValue" #-}
				   applyValueT x1t x2t
	case mResultTE of
	 Just (appT, appE)
	  -> let x'		= XApp x1' x2' tPure
		 xE		= makeTSum kEffect  [x1e, x2e, appE]
		 xC		= makeTSum kClosure [x1c, x2c]
	     in
		trace 	("reconX[XApp]: (" % x1 % " $ ..)\n"
	     		% "    appT:\n"	%> appT	% "\n"
			% "    xE:\n" 	%> xE	% "\n"
			% "    xC:\n"	%> xC	% "\n\n\n") $
	     
		return (x', appT, xE, xC)
	       	
	 _ -> panic stage	
	 	$ "reconX: Type error in value application (x1 x2).\n"
		% "    x1:\n" %> x1	% "\n\n"
		% "    x2:\n" %> x2	% "\n\n"

		% "   T[x1]   = " %> x1t		% "\n\n"
		% "   (flat)  = " %> flattenT x1t	% "\n\n"

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
			$ filter (\c -> case c of
					  TFree v _	-> notElem v vsBind
					  _		-> True)
			$ flattenTSum
			$ makeTSum kClosure sCs)
   
-- match
reconX (XMatch [])
 = panic stage
 	$ "reconX: XMatch has no alternatives\n"

reconX (XMatch aa)
 = do	tt	<- get
	(aa', altTs, altEs, altCs)
			<- fmap unzip4 $ mapM reconA aa

	-- join all the alt types to get the type for the whole match
	--	expression.
	let	Just tMatch	= joinSumTs tt altTs

	return	( XMatch aa'
		, tMatch
		, makeTSum kEffect altEs
		, makeTSum kClosure altCs)

-- var
-- TODO: check against existing annotation.


-- var has no type annotation, so look it up from the table
reconX (XVar v TNil)
 = do	tt		<- get
	let	tM	= Map.lookup v (envEq tt)
	when (isNothing tM)
	 $ panic stage
		$ "reconX: Variable " % v % " has no embedded type annotation and "
		% "is not in the provided environment.\n"
		% "    caller = " % envCaller tt	% "\n"

	let	Just t	= tM
		t'	= inlineTWheresMapT (envEq tt) Set.empty t

		-- When we add the type to this var we need to attach any more constraints associated with it,
		--	else we won't be able to check expressions separate from their enclosing XLAMs
		--	(which carry these constraints)
		vsFree	= freeVars t
		vtsMore	= catMaybes
			$ map (\u -> fmap ((,) u) $ Map.lookup u (envMore tt))
			$ Set.toList vsFree

		tDrop	= makeTFetters t'
				[ FMore (TVar (defaultKindV v) v) t2
				| (v, t2)	<- vtsMore]

	trace ( "reconX[XVar]: dropping type\n"
	      % "    var    = " %> v		% "\n"
	      % "    tDrop  = " %> tDrop	% "\n") $ return ()

	return	( XVar v tDrop
		, tDrop
		, tPure
		, TFree v t)

-- var has a type annotation, so use that as its type
reconX (XVar v t)
 = do	tt	<- get
	let t'	= inlineTWheresMapT (envEq tt) Set.empty t
	return	( XVar v t
		, t'
		, tPure
		, trimClosureC Set.empty Set.empty $ TFree v t)


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
		    XType{}
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
		, [XType r, x]	<- xs
		= do	rx	<- reconX x
			return	( reconBoxType r $ t4_2 $ rx
				, tPure)
		
		-- unboxing
		| MUnbox	<- prim
		, [XType r, x]	<- xs
		= do	rx	<- reconX x
			return	( reconUnboxType r $ t4_2 $ rx
				, TEffect primRead [r])
		  
		-- forcing
		| MForce	<- prim
		, [Just t1]	<- txs
		= do	return	( t1
				, tPure)	
		
		| MTailCall{}	<- prim
		= do	rxs'	<- reconApps xs'
			return	( rxs'
				, tPure)

		| MCall{}	<- prim
		= do	rxs'	<-reconApps xs'
		  	return	( rxs'
				, tPure)

		| MCallApp{}	<- prim
		= do	rxs'	<- reconApps xs'
			return	( rxs'
				, tPure)

		| MApply{}	<- prim
		= do	rxs'	<- reconApps xs'
			return	( rxs'
				, tPure)

		| MCurry{}	<- prim
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
 = do	let	tcLit	= tyConOfLiteralFmt litFmt
		tLit	= TCon tcLit
	return	( xx
		, tLit
		, tPure
		, tEmpty)


-- no match
reconX xx
 = do	tt	<- get
	panic stage 
	 	$ "reconX: no match for " % show xx	% "\n"
		% "    caller = " % envCaller tt	% "\n"


-- | Convert this boxed type to the unboxed version
reconBoxType :: Region -> Type -> Type
reconBoxType r tt
	| Just (v, k, _)		<- takeTData tt
	, (baseName, mkBind, fmt)	<- splitLiteralVarBind (Var.varId v)
	, Just fmtBoxed			<- dataFormatBoxedOfUnboxed fmt
	= makeTData 
		(primVarFmt NameType 
				(pprStrPlain (Var.nameModuleId v) ++ "." ++ baseName) 
				mkBind fmtBoxed)
		(KFun kRegion kValue) 
		[r]


-- | Convert this type to the unboxed version
reconUnboxType :: Region -> Type -> Type
reconUnboxType r1 tt
	| Just (v, k, [r2@(TVar kV _)])	<- takeTData tt
	, kV == kRegion
	, r1 == r2
	, (baseName, mkBind, fmt)	<- splitLiteralVarBind (Var.varId v)
	, Just fmtUnboxed		<- dataFormatUnboxedOfBoxed fmt
	= makeTData
		(primVarFmt NameType 
				(pprStrPlain (Var.nameModuleId v) ++ "." ++ baseName)
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
reconOpApp :: Op -> [Exp] -> ReconM (Type, Effect)
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
	, isUnboxedNumericType_varId (Var.varId v)
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

reconApps [XType t]
 =	return t
 
reconApps [x]
 = 	fmap t4_2 $ reconX x

reconApps (XType t1 : XType t2 : xs)
 = do	table		<- get
	Just t12	<- applyTypeT t1 t2
	reconApps (XType t12 : xs)
 	
reconApps (x1 : XType t2 : xs)
 = do	table		<- get
	t1		<- fmap t4_2 $ keepState $ reconX x1
	Just t12	<- applyTypeT t1 t2
	reconApps (XType t12 : xs)

reconApps (XType t1 : x2 : xs)
 = do	table		<- get
	t2		<- fmap t4_2 $ keepState $ reconX x2
 	Just (t12, _)	<- applyValueT t1 t2
	reconApps (XType t12 : xs)

reconApps (x1 : x2 : xs)
 = do	table		<- get
	t1		<- fmap t4_2 $ keepState $ reconX x1
	t2		<- fmap t4_2 $ keepState $ reconX x2
	Just (t12, _)	<- applyValueT t1 t2
	reconApps (XType t12 : xs)


-- Stmt --------------------------------------------------------------------------------------------

-- | running reconS also adds a type for this binding into the table
reconS	:: Stmt -> ReconM (Stmt, Type, Effect, Closure)

reconS (SBind Nothing x)	
 = do	tt			<- get
	(x', xT, xE, xC)	<- keepState $ reconX x

	let xAnnot	| envDropStmtEff tt	= XTau xE x'
			| otherwise		= x'

	return	( SBind Nothing xAnnot
		, xT
		, xE
		, xC)

reconS (SBind (Just v) x)
 = do	tt	<- get
	(x', xT, xE, xC)	<- reconX x
	put	$ addEqVT v xT tt

	let	xAnnot	| envDropStmtEff tt	= XTau xE x'
			| otherwise		= x'

	return	( SBind (Just v) xAnnot
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
--
reconG	:: Guard -> ReconM (Guard, [Var], Effect, Closure)

reconG gg@(GExp p x)
 = do	tt		<- get
	(x', tX, eX, cX)<- reconX x
	let	binds	= slurpVarTypesW tX p

		-- Work out the effect of testing the case object.
		tX_shape	= stripToShapeT tX

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
			= TEffect primRead [TVar kRegion rH]

			-- object does not have a primary region, assume it is constant
			| otherwise
			= tEmpty

			| otherwise
			= panic stage
			$ "reconG: no match for:\n"
			% "  p        = " % p % "\n"
			% "  tX_shape = " % tX_shape	% "\n"

	put	$ foldr (uncurry addEqVT) tt binds

	trace 	( "reconG:\n"
		% "    gg      = " % gg		% "\n"
		% "    effTest = " % effTest	% "\n") $ return ()
	return	( GExp p x'
		, map fst binds
	   	, makeTSum kEffect ([eX, effTest])
		, cX)

slurpVarTypesW tRHS (WVar v)		= [(v, tRHS)]
slurpVarTypesW tRHS (WLit{})		= []
slurpVarTypesW tRHS (WCon _ v lvt)	= map (\(l, v, t)	-> (v, t)) lvt


-- Value \/ Type application functions ---------------------------------------------------------
-- | Work out the result type and latent effect that will result when 
--	an arg is applied to a function with this type.
--
applyValueT 
	:: Type 		-- ^ type of function
	-> Type 		-- ^ type of arg
	-> ReconM (Maybe 		
			( Type		-- result type
			, Effect))	-- effect caused

applyValueT t1 t2
 = do	table	<- get
	applyValueT' (flattenT t1) (flattenT t2)

applyValueT' t1@(TFetters t1Shape fs) t2
 = do	table	<- get
	let	([[], fsMore], [])
 			= partitionFs [isFWhere, isFMore] fs

	put $ foldr addMoreF table fsMore
	applyValueT' t1Shape t2
 
applyValueT' t0 t3	
 | Just (t1, t2, eff, clo)	<- takeTFun t0
 = do	table	<- get
	let	result
		 | t1 == t3
		 = Just (t2, eff)

		 -- We're currenly using the subsumption judgement more than we really need to.

		 | subsumes (envMore table) t1 t3
		 = {- Debug.Trace.trace 
			(pprStrPlain $ "used subsumption\n" 
			% "t1      = " % t1 % "\n"
			% "t3      = " % t3 % "\n"
			% "envMore = " % envMore table % "\n\n")
		 $ -} Just (t2, eff)

		 | otherwise
		 = freakout stage
			( "applyValueT: Type error in value application.\n"
			% "    called by = " % envCaller table	% "\n\n"
			% "    can't apply argument:\n"	%> t3 % "\n\n"
			% "    to:\n"        		%> t0 % "\n"
			% "\n"
			% "    as it is not <: than:\n"	%> t1 % "\n"
			% "\n"
			% "    with bounds: \n"
			% ("\n" %!% (map (\(v, b) -> "        " % v % " :> " % b) 
			$ Map.toList (envMore table)) % "\n\n")
			) Nothing
	  in	return result


applyValueT' t1 t2
	= freakout stage
		( "applyValueT: No match for (t1 t2).\n"
		% "    t1 = " % t1	% "\n"
		% "    t2 = " % t2	% "\n")
		$ return Nothing
	
	
-- | Apply a value argument to a forall\/context type, yielding the result type.
--	TODO: check that the kinds\/contexts match as we apply.
--
applyTypeT :: Type -> Type -> ReconM (Maybe Type)
applyTypeT t1 t2
 = do	table	<- get
	return $ applyTypeT' table t1 t2

applyTypeT' :: Env -> Type -> Type -> Maybe Type
applyTypeT' table (TForall (BVar v) k t1) t2
	| Just k == kindOfType t2
	= Just (substituteT (Map.insert v t2 Map.empty) t1)

	| otherwise
	= freakout stage
		( "applyTypeT: Kind error in type application.\n"
		% "    caller = " % envCaller table	% "\n"
		% "    in application:\n"
			%> "(\\/ " % parens (v % " :: " % k) % " -> ...)" <> parens t2 %"\n"
		% "\n"
		% "        type: " % t2		% "\n"
		% "    has kind: " % kindOfType t2 	% "\n\n"
		
		% "    expected: " % k	% "\n\n")
		$ Nothing

applyTypeT' table (TForall (BMore v tB) k t1) t2
	-- if the constraint is a closure then trim it first
	| k == kClosure
	, subsumes (envMore table) 
			(flattenT $ trimClosureC Set.empty Set.empty t2) 
			(flattenT $ trimClosureC Set.empty Set.empty tB)
	= Just (substituteT (Map.insert v t2 Map.empty) t1)

	-- check that the constraint is satisfied
	| subsumes (envMore table) t2 tB
	= Just (substituteT (Map.insert v t2 Map.empty) t1)
	
	| otherwise
	= freakout stage
		( "applyTypeT: Kind error in type application.\n"
		% "    caller = " % envCaller table % "\n"
		% "    in application: (\\/ " % v % " :> (" % tB % ") " % k % " -> ...)" % " (" % t2 % ")" % "\n"
		% "\n"
		% "        type: "  % t2 % "\n"
		% "\n"
		% "    is not :> " % tB % "\n"
		% "\n")

		$ Nothing
	

applyTypeT' table t1@(TContext k11 t12) t2
	-- witnesses must match
	| Just k2	<- kindOfType t2
	, packK k11 == packK k2
	= Just t12

	-- kinds don't match
	| Just k2	<- kindOfType t2
	= freakout stage
		( "applyTypeT: Kind error in type application.\n"
		% "    caller = " % envCaller table	% "\n"
		% "    can't apply\n"	%> t2		% "\n\n"
		% "    to\n"		%> t1		% "\n\n"
		% "    k11\n"		%> (packK k11)	% "\n\n"
		% "    K[t2]\n"		%> (packK k2)	% "\n\n")
		$ Nothing


applyTypeT' table (TFetters t1 fs) t
	| Just t1'	<- applyTypeT' table t1 t
	= Just $ TFetters t1' fs
	
applyTypeT' table t1 t2
	= freakout stage
		( "applyTypeT: Kind error in type application.\n"
		% "    caller = " % envCaller table	% "\n"
		% "    can't apply\n"		%> t2	% "\n\n"
		% "    to\n"			%> t1	% "\n\n")
		$ Nothing

	
-- Mask effects ------------------------------------------------------------------------------------

maskE	:: Type -> Effect -> Closure -> Effect

maskE xT xE xC
 = let	fvT		= freeVars xT
	fvC		= freeVars xC

	-- mask non-observable effects that are not in the
	--	environment (closure) or type of the return value.
	xE_masked	= maskReadWriteNotIn (Set.union fvT fvC) xE

	-- TODO: We need to flatten the closure before trimming to make sure effect annots
	--	on type constructors are not lost. It would be better to modify trimClosureC
	--	so it doesn't lose them, or the closure equivalence rule so it doesn't care.
	xE'		= packT xE_masked

   in	xE'


-- Clamp -------------------------------------------------------------------------------------------
-- | Clamp a sum by throwing out any elements of the second one that are not members of the first.
--	Result is at least as big as t1.

clampSum :: Type -> Type -> ReconM Type
clampSum t1 t2
	| kindOfType t1 == kindOfType t2
	= do	table	<- get
		let	parts2		= flattenTSum t1
			parts_clamped	= [p	| p <- parts2
						, subsumes (envMore table) t1 p]
			Just k1		= kindOfType t1

		return $ makeTSum k1 parts_clamped




-- Bits --------------------------------------------------------------------------------------------

pprBounds more
 	= "\n" %!% (map (\(v, b) -> "        " % v % " :> " % b) $ Map.toList more) % "\n"
