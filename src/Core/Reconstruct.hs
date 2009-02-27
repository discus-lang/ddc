-- Core.Reconstruct
--
--	Reconstruct and check the type/region/effect/closure and witness information in
--	the core IR.
--
-- 	The types of free variables can be supplied either as annotations on the variables themselves, 
--	or supplied via a table. Any missing annotations are filled during checking, so expressions
--	returned may be checked again without using the table.
--	
--	The table also carries the name of the calling function, to print in panic messages incase
--	a type error is uncoverred.
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

import Type.Util		hiding (flattenT, trimClosureC)
import Type.Util.Environment
import Type.Builtin

import Shared.Pretty
import Shared.Error
import Shared.VarPrim
import Shared.Literal
import Shared.Base
import Util.Graph.Deps
import Util

import qualified Shared.Var	as Var
import Shared.Var		(NameSpace(..))

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Debug.Trace	as Debug


-----
stage	= "Core.Reconstruct"

debug	= False
trace ss x	
	= if debug 
		then Debug.trace (pprStrPlain ss) x 
		else x

-- Tree --------------------------------------------------------------------------------------------
reconTree'
	:: String	-- caller name
	-> Tree		-- header tree 
	-> Tree 	-- core tree
	-> Tree		-- core tree with reconstructed type information
reconTree' caller tHeader tCore
 = reconTree
 	emptyEnv { envCaller = Just caller }
	tHeader tCore

reconTree
	:: Env	
	-> Tree		-- header tree 
	-> Tree 	-- core tree
	-> Tree		-- core tree with reconstructed type information
	
reconTree table tHeader tCore
 = {-# SCC "reconstructTree" #-}
   let	-- slurp out all the stuff defined at top level
	topTypes	= {-# SCC "reconTree/topTypes" #-} catMap slurpTypesP (tHeader ++ tCore)
 	tt		= {-# SCC "reconTree/table"    #-} foldr (uncurry addEqVT) table topTypes
	
	-- reconstruct type info on each top level thing
	tCore'		= map (reconP tt) tCore
	
      in tCore'
	

-- Top ---------------------------------------------------------------------------------------------
reconP' :: String -> Top -> (Top, Type, Effect, Closure)
reconP' caller (PBind v x)

 = let	(SBind (Just v') x', typ', eff', clo')
 		= snd 
		$ reconS emptyEnv { envCaller = Just caller } 
		$ SBind (Just v) x
		
   in	(PBind v' x', typ', eff', clo')
 

reconP_type :: String -> Top -> Type
reconP_type caller p
	= t4_2 $ reconP' caller p


reconP	:: Env 
	-> Top 
	-> Top

reconP tt (PBind v x)
 = let	(x', _, _, _)	
 		= {-# SCC "reconP/reconX" #-} reconX tt x
   in	PBind v x'

reconP tt p		= p


-- Expression --------------------------------------------------------------------------------------
reconX' :: String -> Exp -> (Exp, Type, Effect, Closure)
reconX' caller x 
	= reconX emptyEnv { envCaller = Just caller } x

reconX_type :: String -> Exp -> Type
reconX_type caller x
	= t4_2 $ reconX' caller x

reconX 	:: Env
	-> Exp 
	-> ( Exp
	   , Type, Effect, Closure)

-- LAM
reconX tt xx@(XLAM b@(BMore v t1) t2 x)
 = let	tt'			= addMoreVT v t1 tt
 	(x', xT, xE, xC)	= reconX tt' x

   in 
{-   	trace 	("(/\\ " % b % " -> ...)\n"
   		% "  xT:\n" %> xT	% "\n"
		% "  xC:\n" %> xC	% "\n\n") $ -}

	( XLAM b t2 x'
	, TForall b t2 xT
	, xE	  
	, xC)

reconX tt (XLAM v k@KClass{} x)
 = let	(x', xT, xE, xC)	= reconX tt x
   in 
{-    trace 	("(/\\ " % v % " -> ...)\n"
   		% "  xT:\n" %> xT	% "\n"
		% "  xC:\n" %> xC	% "\n\n") $ -}
      	( XLAM 	   v k x'
    	, TContext k xT 
	, xE
	, xC)

reconX tt (XLAM v k x)
 = let	(x', xT, xE, xC)	= reconX tt x
   in 
{-      trace 	("(/\\ " % v % " -> ...)\n"
   		% "  xT:\n" %> xT	% "\n"
		% "  xC:\n" %> xC	% "\n\n") $ -}

   	( XLAM 	  v k x'
    	, TForall v k xT 
	, xE
	, xC)
 
-- APP

-- handle applications to string literals directly
--	this way we don't need to invent a variable for the \/ bound region 
--	in the literal's type scheme. String literals never appear without their region
--	parameters in the core so this is ok.

reconX tt exp@(XAPP (XLit (LiteralFmt lit fmt)) (TVar KRegion r))
 = let	t	= case lit of
 			LString{}	-> makeTData (primTString Unboxed) (KFun KRegion KValue) [TVar KRegion r]
			_		-> panic stage
					$  "reconX/XApp: non string constant applied to region\n"
					%  "    exp = " % exp	% "\n"
   in	( exp
   	, t
	, pure
	, empty)


reconX tt exp@(XAPP x t)
 = let	(x', tX, eX, cX)	= reconX tt x
   in	
   	case applyTypeT tt tX t of
   	 Just tApp
	  -> {- trace 	("(" % x % " @ " % t % ")\n"
	     		% "    tApp:\n"	%> tApp		% "\n"
			% "    eX:\n" 	%> eX		% "\n"
			% "    cX:\n"	%> cX		% "\n\n\n") $ -}
	     	  
		( XAPP x' t
		, tApp
		, eX
		, cX)
	  
	 _ -> panic stage
	 	$ " reconX: Kind error in type application (x t).\n"
		% "     caller = "  % envCaller tt	% "\n"
		% "     x      =\n" %> x		% "\n\n"
		% "     t      =\n" %> t		% "\n\n"
		% "   T[x]     =\n" %> tX	% "\n\n"

-- Tet
reconX tt (XTet vts x)
 = let	tt'			= foldr (uncurry addEqVT) tt vts
 	(x', tx, xe, xc)	= reconX tt' x
   in	( XTet   vts x'
   	, TFetters tx 
		[ FWhere (TVar (defaultKindV v) v) t2
			| (v, t2)	<- vts]
	, xe
	, xc)
   
-- xtau
-- We can't actually check the reconstructed type against the annotation here
--	because we can't see /\ bound TREC variables that might be bound above us.
--
--	eg, with /\ t -> [** t] x
--	    the type of x is (forall t. t), not just t.
--
-- 	The XTau types are checked by reconS instead.
--
reconX tt exp@(XTau tauT x)
 = let	(x', xT, xE, xC)	= reconX tt x
   in	( XTau tauT x'
	, xT
	, xE
	, xC)


-- lam
reconX tt exp@(XLam v t x eff clo)
 = {-# SCC "reconX/XLam" #-}
   let	reconX_lam
 
	 	| tt'			<- addEqVT v t tt
		, (x', xT, xE, xC)	<- reconX tt' x

		, eff'		<- packT $ substituteT (envEq tt) eff
		, clo_sub	<- packT $ substituteT (envEq tt) clo
		
		, fvT		<- freeVars xT
		, fvC		<- freeVars xC
		
		-- mask non-observable effects that are not in the
		--	environment (closure) or type of the return value.
		, xE_masked	<- maskReadWriteNotIn (Set.union fvT fvC) xE

		-- TODO: We need to flatten the closure before trimming to make sure effect annots
		--	on type constructors are not lost. It would be better to modify trimClosureC
		--	so it doesn't loose them, or the closure equivalence rule so it doesn't care.
		, xC_masked	<- makeTMask KClosure xC (TTag v)
		, xC_flat	<- flattenT xC_masked
		, xC'		<- trimClosureC Set.empty Set.empty $ xC_flat

		, xE'		<- packT xE_masked
	
		-- check effects match
		, () <- if subsumes (envMore tt) eff' xE'
			 then ()
			 else panic stage
				$ "reconX: Effect error in core.\n"
				% "    caller = " % envCaller tt	% "\n"
				% "    in lambda abstraction:\n" 	%> exp	% "\n\n"
				% "    reconstructed effect of body:\n" %> xE'	% "\n\n"
				% "    is not <: annot on lambda:\n"	%> eff'	% "\n\n"
				% "    with bounds:\n"
				% "    t:       " %> t	 % "\n"
				% "    env:     " %> xC  % "\n"
				% "    fv(Env): " %> fvC % "\n"
				% "    fv(T):   " %> fvT % "\n"  
				
				% pprBounds (envMore tt)
				
		-- check closures match
		, () <- if subsumes (envMore tt) clo_sub xC'
			 then ()
			 else ()
{-				panic stage
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
		--	If we reduced the annotation too far, ie so it wasn't >= than the reconstructed
		--	effect\/closure then we would get an error next type we called reconstruct.
		--
		--	We actually /have/ to do this clamping because when Core.Bind introduces local
		--	regions for mutually recursive functions, the left over effect annotations contain
		--	regions which are out of scope which makes Core.Lint complain.
		--	
		--	Another way of looking at this is that we're doing the effect masking that Core.Bind
		--	should have done originally.
		--
		, eff_clamped	<- clampSum tt xE' eff'

		-- don't clamp closures. There is no need to, and clampSum gives the wrong answer
		--	because two closures   (x : Int %r1) and (y : Int %r1) are taken to be non-equal
		--	due to their differing tags.
--		, clo_clamped	<- clampSum tt xC' clo_sub

		= trace ( "reconX: XLam\n"
			% "    xT           = " % xT	% "\n"
			% "    xE'  (recon) = " % xE'	% "\n"
			% "    eff' (annot) = " % eff'	% "\n"
			% "    eff_clamped  = " % eff_clamped	% "\n"
			% "    fvT          = " % fvT		% "\n"
			% "    fvC          = " % fvC		% "\n"
			% "    xE           = " % xE		% "\n"
			% "    xE_masked    = " % xE_masked     % "\n"
		   	% "    clo          = " % xC		% "\n") $

		   ( XLam v t x' eff_clamped clo_sub
		   , makeTFun t xT eff_clamped clo_sub
		   , TBot KEffect
		   , xC')

   in	reconX_lam

-- local
-- TODO: check well foundedness of witnesses
-- 	Note that for performance reasons, we don't mask effects local effects here.
--	All the local effects from the body of a lambda abstraction are masked
--	all at once in the rule for XLam.
--
reconX tt (XLocal v vs x)
 = let	(x', xT, xE, xC)	= reconX tt x

	-- not sure if we should worry about regions variables escaping here.
	-- we're not doing stack allocation, so it doesn't matter if they do.
   in	{- (if Set.member v (freeVars xT) 
   		then panic stage 
			( "reconX: region " % v % " is not local\n"
			% "    caller = " % envCaller tt	% "\n"
			% "    t      = " % xT	% "\n"
			% "    x      = " % x'	% "\n\n")
			id
		else	id) -}
	 
   	( XLocal v vs x'
   	, xT
	, xE
	, xC)
	
-- app
reconX tt exp@(XApp x1 x2 eff)
 = let	(x1', x1t, x1e, x1c)	= reconX tt x1
	(x2', x2t, x2e, x2c)	= reconX tt x2
	mResultTE		= {-# SCC "reconX/applyValue" #-}
	                          applyValueT tt x1t x2t
   in	case mResultTE of
   	 Just (appT, appE)
  	  -> let x'		= XApp x1' x2' pure
	  	 xE		= makeTSum KEffect  [x1e, x2e, appE]
		 xC		= makeTSum KClosure [x1c, x2c]

     	     in {- trace 	("(" % x1 % " $ ..)\n"
	     		% "    appT:\n"	%> appT	% "\n"
			% "    xE:\n" 	%> xE	% "\n"
			% "    xC:\n"	%> xC	% "\n\n\n") $ -}
	     
		     	( x'
		        , appT
			, xE
			, xC)
	       	
	 _ -> panic stage	
	 	$ "reconX: Type error in value application (x1 x2).\n"
		% "    x1:\n" %> x1	% "\n\n"
		% "    x2:\n" %> x2	% "\n\n"

		% "   T[x1]   = " %> x1t		% "\n\n"
		% "   (flat)  = " %> flattenT x1t	% "\n\n"
		

		% "   T[x2]   = " % x2t		% "\n\n"
   
-- do
reconX tt (XDo ss)
 = let	(_, sts)		= mapAccumL reconS tt ss
	(ss', sTs, sEs, sCs)	= unzip4 sts
	Just t			= takeLast sTs
	vsBind			= catMaybes $ map takeVarOfStmt ss'
	
   in	( XDo ss'
        , t
	, makeTSum KEffect sEs
	, trimClosureC Set.empty Set.empty
		$ makeTMask 
			KClosure
			(makeTSum KClosure (map ((trimClosureC Set.empty Set.empty) . flattenT) sCs))
			(makeTSum KClosure (map TTag vsBind)) )
   
-- match
reconX tt (XMatch [])
 = panic stage
 	$ "reconX: XMatch has no alternatives\n"

reconX tt (XMatch aa)
 = let	(aa', altTs, altEs, altCs)	
			= unzip4 $ map (reconA tt) aa
 	
	-- join all the alt types to get the type for the
	--	whole match expression.
	Just tMatch	= joinSumTs tt altTs

   in	( XMatch aa'
   	, tMatch
	, makeTSum KEffect altEs
	, makeTSum KClosure altCs )

-- var
-- TODO: check against existing annotation.


-- var has no type annotation, so look it up from the table
reconX tt (XVar v TNil)
	| Just t	<- Map.lookup v (envEq tt)
	, t'		<- inlineTWheresMapT (envEq tt) Set.empty t 

	-- When we add the type to this var we need to attach any more constraints associated with it, 
	--	else we won't be able to check expressions separate from their enclosing XLAMs 
	--	(which carry these constraints)
	, vsFree	<- freeVars t
	, vtsMore	<- catMaybes
			$  map (\u -> case Map.lookup u (envMore tt) of
						Nothing	-> Nothing
						Just t	-> Just (u, t))
			$ Set.toList vsFree

	, tDrop		<- makeTFetters t' 
				[ FMore (TVar (defaultKindV v) v) t2
					| (v, t2)	<- vtsMore]

{-	= trace ( "reconX[XVar]: dropping type\n"
		% "    var    = " %> v		% "\n"
		% "    tDrop  = " %> tDrop	% "\n")
	   $	
-}
	=   ( XVar v tDrop
	    , tDrop
	    , TBot KEffect
	    , TFree v t)
	  
	| otherwise
	= panic stage 
	 	$ "reconX: Variable " % v % " has no embeded type annotation and is not in the provided environment.\n"
		% "    caller = " % envCaller tt	% "\n"

	
-- var has a type annotation, so use that as its type
reconX tt (XVar v t)
 = let	t'	= inlineTWheresMapT (envEq tt) Set.empty t
   in	( XVar v t
	, t'
	, TBot KEffect
	, trimClosureC Set.empty Set.empty $ TFree v t)


-- prim
--	BUGS: 	effects from primitive applications are not generated
--		this isn't a problem at the momement because we don't check effect information
--		after Core.Curry
--
reconX tt xx@(XPrim prim xs)
-- = trace ("reconX[XPrim]: " % xx % "\n")
 = let	
 	-- some of the xs are type terms which we can't call recon on
	--	so we have to do some contortions to get the closure from the others.
	reconMaybeX tt x
	 = case x of
	 	XType{}	-> (x, Nothing, Nothing, Nothing)
		_	->
		 let (x', typ, eff, clo)	= reconX tt x
		 in  (x', Just typ, Just eff, Just clo)
		 
 
  	(xs', txs, xsmEs, xsmCs)		
 		= unzip4 $ map (reconMaybeX tt) xs

	-- work out the result type an effect of applying the primitive operator
	(tPrim, ePrim)	

		-- boxing		
		| MBox 		<- prim
		, [XType r, x]	<- xs
		= ( reconBoxType r $ t4_2 $ reconX tt x
		  , pure)
		
		-- unboxing
		| MUnbox	<- prim
		, [XType r, x]	<- xs
		= ( reconUnboxType r $ t4_2 $ reconX tt x
		  , TEffect primRead [r])
		  
		-- forcing
		| MForce	<- prim
		, [Just t1]	<- txs
		= ( t1
		  , pure)	
		
		| MTailCall{}	<- prim
		= ( reconApps tt xs'
		  , pure)

		| MCall{}	<- prim
		= ( reconApps tt xs'
		  , pure)

		| MCallApp{}	<- prim
		= ( reconApps tt xs'
		  , pure)

		| MApply{}	<- prim
		= ( reconApps tt xs'
		  , pure)

		| MCurry{}	<- prim
		= ( reconApps tt xs'
		  , pure)

		| MFun{}	<- prim
		= ( reconApps tt xs'
		  , pure)

		| MOp op	<- prim
		= reconOpApp tt op xs'

		| otherwise
		= panic stage
		$ "reconX/Prim: no match for " % prim <> punc " " xs % "\n"
		
   in	( XPrim prim xs'
   	, tPrim
   	, makeTSum KEffect  $ ePrim : catMaybes xsmEs
	, makeTSum KClosure $ catMaybes xsmCs)


reconX tt xx@(XLit litFmt)
 = let	tcLit	= tyConOfLiteralFmt litFmt
	tLit	= TCon tcLit
   in	( xx
	, tLit
	, pure
	, empty)


-- no match
reconX tt xx
 	= panic stage 
 	$ "reconX: no match for " % show xx	% "\n"
	% "    caller = " % envCaller tt	% "\n"


-- | Convert this boxed type to the unboxed version
reconBoxType :: Region -> Type -> Type
reconBoxType r tt
	| Just (v, k, _)		<- takeTData tt
	, (baseName, mkBind, fmt)	<- splitLiteralVarBind (Var.bind v)
	, Just fmtBoxed			<- dataFormatBoxedOfUnboxed fmt
	= makeTData 
		(primVarFmt NameType baseName mkBind fmtBoxed)
		(KFun KRegion KValue) 
		[r]


-- | Convert this type to the unboxed version
reconUnboxType :: Region -> Type -> Type
reconUnboxType r1 tt
	| Just (v, k, [r2@(TVar KRegion _)])	
			<- takeTData tt
	, r1 == r2
	, (baseName, mkBind, fmt)	<- splitLiteralVarBind (Var.bind v)
	, Just fmtUnboxed		<- dataFormatUnboxedOfBoxed fmt
	= makeTData 
		(primVarFmt NameType baseName mkBind fmtUnboxed)
		KValue
		[]


-- | Split the VarBind for a literal into its components, 
--	eg  TInt fmt -> ("Int", TInt, fmt)
splitLiteralVarBind 
	:: Var.VarBind 
	-> ( String
	   , DataFormat -> Var.VarBind
	   , DataFormat)

splitLiteralVarBind bind
 = case bind of
 	Var.TBool  fmt	-> ("Bool",   Var.TBool,   fmt)
	Var.TWord  fmt	-> ("Word",   Var.TWord,   fmt)
	Var.TInt   fmt	-> ("Int",    Var.TInt,    fmt)
	Var.TFloat fmt	-> ("Float",  Var.TFloat,  fmt)
	Var.TChar  fmt	-> ("Char",   Var.TChar,   fmt)
	Var.TString fmt	-> ("String", Var.TString, fmt)



-- | Reconstruct the type and effect of an operator application
--	Not sure if doing this manually is really a good way to do it.
--	It'd be nice to have a more general mechanism like GHC rewrite rules..
reconOpApp :: Env -> Op -> [Exp] -> (Type, Effect)
reconOpApp tt op xs

	-- arithmetic operators
	| op == OpNeg
	, [t1]		<- map (t4_2 . reconX tt) xs
	, isUnboxedNumericType t1
	= (t1, pure)

	| elem op [OpAdd, OpSub, OpMul, OpDiv, OpMod]
	, [t1, t2]	<- map (t4_2 . reconX tt) xs
	, isUnboxedNumericType t1
	, t1 == t2
	= (t1, pure)
	
	-- comparison operators
	| elem op [OpEq, OpNeq, OpGt, OpGe, OpLt, OpLe]
	, [t1, t2]	<- map (t4_2 . reconX tt) xs
	, isUnboxedNumericType t1
	, t1 == t2
	= (makeTData (primTBool Unboxed) KValue [], pure)

	-- boolean operators
	| elem op [OpAnd, OpOr]
	, [t1, t2]		<- map (t4_2 . reconX tt) xs
	, Just (v, k, [])	<- takeTData t1
	, v == (primTBool Unboxed)
	, t1 == t2
	= (makeTData (primTBool Unboxed) KValue [], pure)
	
	| otherwise
	= panic stage
	$ "reconOpApp: no match for " % op % " " % xs % "\n"


-- | Checks whether a type is an unboxed numeric type
isUnboxedNumericType :: Type -> Bool
isUnboxedNumericType tt
 	| Just (v, _, []) <- takeTData tt
	, isUnboxedNumericType_bind (Var.bind v)
	= True

	-- treat pointers as numeric types
	| Just (v, _, _) <- takeTData tt
	, v == primTPtrU
	= True
	
	| otherwise
	= False

isUnboxedNumericType_bind bind
 = case bind of
 	Var.TWord _	-> True
	Var.TInt _	-> True
	Var.TFloat _	-> True
	_		-> False


-- | Reconstruct the result type when this list of expressions
--	is applied in a left to right order.
--
--	eg  [x1, x2, x3, x4] =>  ((x1 x2) x3) x4
--
reconApps 
	:: Env
	-> [Exp] 
	-> Type

reconApps table [XType t]
 =	t
 
reconApps table [x]
 = 	t4_2 $ reconX table x

reconApps table (XType t1 : XType t2 : xs)
 = let	Just t12	= applyTypeT table t1 t2
   in	reconApps table (XType t12 : xs)
 	
reconApps table (x1 : XType t2 : xs)
 = let	t1		= t4_2 $ reconX table x1
 	Just t12	= applyTypeT table t1 t2
   in	reconApps table (XType t12 : xs)

reconApps table (XType t1 : x2 : xs)
 = let	t2		= t4_2 $ reconX table x2
 	Just (t12, _)	= applyValueT table t1 t2
   in	reconApps table (XType t12 : xs)

reconApps table (x1 : x2 : xs)
 = let	t1		= t4_2 $ reconX table x1
	t2		= t4_2 $ reconX table x2
 	Just (t12, _)	= applyValueT table t1 t2
   in	reconApps table (XType t12 : xs)


-- Stmt --------------------------------------------------------------------------------------------

-- | running reconS also adds a type for this binding into the table
reconS 	:: Env 
	-> Stmt 
	-> (Env, (Stmt, Type, Effect, Closure))

reconS tt (SBind Nothing x)	
 = let	(x', xT, xE, xC)	= reconX tt x

	xAnnot 	| envDropStmtEff tt	= XTau xE x'
		| otherwise		= x'
	
   in	( tt
   	, ( SBind Nothing xAnnot
	  , xT
	  , xE
	  , xC))

reconS tt (SBind (Just v) x)
 = let	(x', xT, xE, xC)	= reconX tt x
	tt'			= addEqVT v xT tt

	xAnnot	| envDropStmtEff tt	= XTau xE x'
		| otherwise		= x'

   in	( tt'
   	, ( SBind (Just v) xAnnot
	  , xT
	  , xE
	  , TMask KClosure xC (TTag v)))
	  

-- Alt ---------------------------------------------------------------------------------------------
reconA 	:: Env 
	-> Alt 
	-> (Alt, Type, Effect, Closure)

reconA tt (AAlt gs x)
 = let	(tt', gecs)		= mapAccumL reconG tt gs
	(gs', vssBind, gEs, gCs)= unzip4 gecs
	(x', xT, xE, xC)	= reconX tt' x
   in	( AAlt gs' x'
   	, xT
	, makeTSum KEffect  (gEs ++ [xE])
	, makeTMask 
		KClosure
		(makeTSum 
			KClosure 
			(gCs ++ [xC]))
		(makeTSum
			KClosure
			(map TTag $ concat vssBind)))
  
 
-- Guards ------------------------------------------------------------------------------------------

-- | running reconG also adds a types for the matched variables into the table.

-- TODO: check type of pattern against type of expression
--
reconG	:: Env 
	-> Guard 
	-> ( Env
	   , (Guard, [Var], Effect, Closure))

reconG tt gg@(GExp p x)
 = let	
	(x', tX, eX, cX)= reconX tt x
	binds		= slurpVarTypesW tX p
 	tt'		= foldr (uncurry addEqVT) tt binds
	
	-- Work out the effect of testing the case object.
	tX_shape	= stripToShapeT tX

	effTest	
		-- If the LHS of the guard is just a var then there is no 'match' per se, and no effects.
		| WVar{}		<- p
		= TBot KEffect

		-- If the type of the object has no regions we assume that
		--	it is constant, so matching against it generates no effects.
		| Just (vD, _, [])	<- takeTData tX_shape
		= TBot KEffect

		-- matching against some object cause a read effect on its primary region.
		| Just (vD, _, TVar KRegion rH : _)
					<- takeTData tX_shape
		= TEffect primRead [TVar KRegion rH]

		-- object does not have a primary region, assume it is constant
		| otherwise
		= TBot KEffect
		
{-		| otherwise
		= panic stage 
			$ "reconG: no match for:\n"
			% "  p        = " % p % "\n"
			% "  tX_shape = " % tX_shape	% "\n"
-}			

   in {- trace 	( "regonG\n"
		% "    gg      = " % gg		% "\n"
		% "    effTest = " % effTest	% "\n") $ -}
   	( tt'
   	, ( GExp p x'
	  , map fst binds
   	  , makeTSum KEffect ([eX, effTest])
	  , cX))
 
slurpVarTypesW tRHS (WVar v)		= [(v, tRHS)]
slurpVarTypesW tRHS (WLit{})		= []
slurpVarTypesW tRHS (WCon v lvt)	= map (\(l, v, t)	-> (v, t)) lvt




-- Value / Type application functions ---------------------------------------------------------


-- | Work out the result type and latent effect that will result when 
--	an arg is applied to a function with this type.
--
applyValueT 
	:: Env		-- ^ table of constraints
	-> Type 		-- ^ type of function
	-> Type 		-- ^ type of arg
	-> Maybe 		
		( Type		-- result type
		, Effect)	-- effect caused

applyValueT table t1 t2
 =	applyValueT' table (flattenT t1) (flattenT t2)

applyValueT' table t1@(TFetters t1Shape fs) t2
 = let	([[], fsMore], [])
 		= partitionFs [isFWhere, isFMore] fs
	
	table'	= foldr addMoreF table fsMore
	
   in	applyValueT' table' t1Shape t2
 
applyValueT' table t0 t3	
	| Just (t1, t2, eff, clo)	<- takeTFun t0
	= if subsumes (envMore table) t1 t3
		then Just (t2, eff)
		else freakout stage
			( "applyValueT: Type error in value application.\n"
			% "    called by = " % envCaller table	% "\n\n"
			% "    can't apply argument:\n"	%> t3 % "\n\n"
			% "    to:\n"        		%> t0 % "\n"
			% "\n"
			% "    as it is not <: than:\n"	%> t1 % "\n"
			% "\n"
			% "    with bounds: \n"
			% ("\n" %!% (map (\(v, b) -> "        " % v % " :> " % b) 
				$ Map.toList (envMore table)) % "\n\n"))
			$ Nothing
	
applyValueT' _ t1 t2
	= freakout stage
		( "applyValueT: No match for (t1 t2).\n"
		% "    t1 = " % t1	% "\n"
		% "    t2 = " % t2	% "\n")
		Nothing
	
	
-- | Apply a value argument to a forall/context type, yielding the result type.
--	TODO: check that the kinds/contexts match as we apply.
--
applyTypeT :: Env -> Type -> Type -> Maybe Type

applyTypeT table (TForall (BVar v) k t1) t2
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

applyTypeT table (TForall (BMore v tB) k t1) t2
	-- if the constraint is a closure then trim it first
	| k == KClosure
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
	

applyTypeT table t1@(TContext k11 t12) t2
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


applyTypeT table (TFetters t1 fs) t
	| Just t1'	<- applyTypeT table t1 t
	= Just $ TFetters t1' fs
	
applyTypeT table t1 t2
	= freakout stage
		( "applyTypeT: Kind error in type application.\n"
		% "    caller = " % envCaller table	% "\n"
		% "    can't apply\n"		%> t2	% "\n\n"
		% "    to\n"			%> t1	% "\n\n")
		$ Nothing

	
-- Clamp -------------------------------------------------------------------------------------------
-- | Clamp a sum by throwing out any elements of the second one that are not members of the first
--	result is at least as big as t1

clampSum :: Env -> Type -> Type -> Type
clampSum table t1 t2
	| kindOfType t1 == kindOfType t2
	= let	parts2		= flattenTSum t1
		parts_clamped	= [p	| p <- parts2
	   				, subsumes (envMore table) t1 p]
		Just k1		= kindOfType t1

	  in	makeTSum k1 parts_clamped




-- Bits --------------------------------------------------------------------------------------------

pprBounds more
 	= "\n" %!% (map (\(v, b) -> "        " % v % " :> " % b) $ Map.toList more) % "\n"
