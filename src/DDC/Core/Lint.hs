{-# OPTIONS -fno-warn-unused-binds -fno-warn-unused-imports #-}

-- | Check for type errors or other problems in a core program, and `panic`
--   if we find and. Also do a deepseq along the way. This module should
--   perform any possible internal consitency check we can think of on the 
--   core program.
--
--   TODO: Do full type checking.
--	   Check syntactic soundness of witnesses.
--	   Check for type vars that are out of scope
--
module DDC.Core.Lint
	( checkGlobs
	, checkExp
	, checkType
	, checkKind
	, Env	(..))
where
import DDC.Core.Lint.Prim
import DDC.Core.Lint.Env
import DDC.Core.Lint.Base
import Shared.VarPrim
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Base.Literal
import DDC.Base.DataFormat
import DDC.Core.Glob
import DDC.Core.Exp
import DDC.Type
import DDC.Var
import Data.List
import Data.Maybe
import Core.Util		(maybeSlurpTypeX)
import Data.Map			(Map)
import qualified Data.Map	as Map
import qualified Debug.Trace

stage		= "Core.Lint"
debug		= True
trace ss x	= if debug then Debug.Trace.trace (pprStrPlain ss) x else x

-- Glob -------------------------------------------------------------------------------------------
checkGlobs :: Glob -> Glob -> ()
checkGlobs cgHeader cgCore 
	= ()
{-	= checkList (checkBind (envInit cgHeader cgCore))
	$ Map.elems
	$ globBind cgCore
-}

-- Top --------------------------------------------------------------------------------------------
checkBind :: Env -> Top -> ()
checkBind env pp
 = case pp of
	PBind v x
	 -> let Just t	= maybeSlurpTypeX x
	    in	checkType' t [] env 
	  	`seq` withType v t env (checkExp x)
		`seq` ()


-- Exp --------------------------------------------------------------------------------------------
-- | Check an expression, returning its type.
checkExp :: Exp -> Env -> (Type, Effect, Closure)
checkExp xx env
 = let	result@(t, eff, clo)	= checkExp_trace xx env
   in	trace (vcat 
		[ "exp = " 	% xx
		, "type:    " 	% t
		, "eff:     "	% eff
		, "clo:     "   % clo])
		result
			
checkExp_trace xx env
 = case xx of
	XNil	-> panic stage "checkExp: XNil"
	
	-- Variables
	XVar v t
	 | varNameSpace v /= NameValue
	 -> panic stage 
		$ "checkExp: invalid namespace for variable " 
		% v <> (show $ varNameSpace v)
	
	 | otherwise
	 -> checkType' t [] env 
	 `seq` (t, tPure, tEmpty)

	-- Type abstraction
	XLAM BNil k x
	 -> checkKind k env 
	 `seq` checkExp x env
	
	XLAM (BVar v) k x
	 -> checkKind k env
	 `seq` withKind v k env (checkExp x)

	-- Value abstraction
	-- TODO check effect and closure annots
	XLam v t x eff clo
	 | varNameSpace v /= NameValue
	 -> panic stage
		$ "checkExp invalid namespace for variable "
		% v <> (show $ varNameSpace v)
		
	 | otherwise
	 ->    checkType t env
	 `seq` checkType eff env
	 `seq` checkType clo env 
	 `seq` let (t', eff', clo')	= withType  v t env (checkExp x)
	       in  if      t   /= t'	then panic stage $ "type mismatch"      --- WRONG
		   else if eff /= eff'	then panic stage $ "effect mismatch"
	           else if clo /= clo'	then panic stage $ "closure mismatch"
		   else (t, eff, clo)

	-- Value application
--	XApp x1 x2 eff
	


	-- Do expression
	XDo ss		-> checkStmts ss env

	-- Match expression
	XMatch aa	-> checkAlts  aa env

	-- Primitive operator.
	XPrim  prim xs	-> checkPrim prim xs env

	-- Type annotation
	XTau t x
	 ->    checkType t env
	 `seq` let result@(t', _, _)	= checkExp x env
	       in if t /= t'		then panic stage $ "type mismatch in xtau"
		  else result
		
	_ -> panic stage 
		$ vcat 	[ "checkExp: no match for " <> xx
			, ppr $ show xx]
		


-- Statements -------------------------------------------------------------------------------------
-- | Check a list of (possibly recursive) statements.
checkStmts :: [Stmt] -> Env -> (Type, Effect, Closure)

-- TODO: need to recursively add types to environment.
checkStmts ss env
	= checkStmts' env ss [] []

checkStmts' _ [] _ _
 	= panic stage
	$ "checkStmts': no statements"
	
checkStmts' env (SBind _ x : []) effAcc cloAcc
 = let	(t, eff, clo)	= checkExp x env
   in	( t
	, makeTSum kEffect  (eff : effAcc)
	, makeTSum kClosure (clo : cloAcc))
	
-- types for all bindings must already be in environment.
checkStmts' env (SBind Nothing x : ss) effAcc cloAcc 
 = let	(_, eff, clo)	= checkExp x env
   in 	checkStmts' env ss (eff : effAcc) (clo : cloAcc)

-- TODO: check type against on already in environment.
checkStmts' env (SBind (Just v) x : ss) effAcc cloAcc
 = let	(_, eff, clo)	= checkExp x env
   in	checkStmts' env ss (eff : effAcc) (clo : cloAcc)


-- Alternatives -----------------------------------------------------------------------------------
-- | Check a list of match alternatives.
-- TODO: handle guards.
-- TODO: add effect from the match.
checkAlts :: [Alt] -> Env -> (Type, Effect, Closure)
checkAlts as env
	= checkAlts' env as [] [] []

checkAlts' env [] types effAcc cloAcc
 = 	( fromMaybe 
		(panic stage $ "checkAlts: can't join types")
		(joinSumTs types)
	, makeTSum kEffect  effAcc
	, makeTSum kClosure cloAcc)

checkAlts' env (AAlt gs x : as) types effAcc cloAcc
 = let	(t, eff, clo)	= checkExp x env
   in	checkAlts' env as (t : types) (eff : effAcc) (clo : cloAcc)


-- Type -------------------------------------------------------------------------------------------
checkType :: Type -> Env -> Kind
checkType tt env 
	= checkType' tt [] env 

-- | Check a type expression, returning its kind.
--	This does a complete check of the entire structure, so is not fast.
--	If you just want to quickly get the kind of a type then use kindOfType instead.
--
checkType' 
	:: Type 	-- Type to check.
	-> [Kind] 	-- Stack of kinds of bound type variables, 
			--	referenced by the De'Bruijn type indicies in
			--	the types of witness constructors.
	-> Env 		-- Type and Kind Environment.
	-> Kind

-- NOTE: All kinds returned by this function must be checked by checkKind at some point.
checkType' tt stack env
 = case tt of
	TNil	-> panic stage $ "TNil should not appear in core types.\n"
	
	TForall b k1 t2
	 -> case b of
		BVar v	
		 -> 	checkKind' k1 stack env
		 `seq`	withKind v k1 env	$!
			checkType' t2 (k1 : stack)
			
		BMore v t1
		 -> 	checkKind' k1 stack env
		 `seq`	withKind  v k1 env 	$! \env'  ->
		   	withBound v t1 env' 	$!
		   	checkType' t2 (k1 : stack)
	
	-- TODO: Add fetters to environment.
	TFetters t1 fs
	 ->	lintList lintF fs env
	 `seq`	checkType' t1 stack env
	
	-- TODO: Add constraints to environment.
	TConstrain t1 crs
	 ->	lintCRS crs env
	 `seq`	checkType' t1 stack env
	
	TApp t1 t2
	 -> case checkType' t1 stack env of
		KFun k11 k12
		 | k11 == checkType' t2 stack env 
		 -> 	  checkKind' k12 stack env
		 `seq`	k12
		
		 | otherwise
		 -> panic stage 
		 $ vcat	[ ppr "Kind error in type application."
			, "    kind:           " % checkType' t2 stack env	
			, "    does not match: " % k11 
			, "    in type:        " % tt ]

		k1 -> panic stage
		 $ vcat [ ppr "Kind error in type application."
			, "    cannot apply type: " % t2
			, "    to type:           " % t1
			, "    which has kind:    " % k1]
		
				
	TSum k ts
	 -> 	checkKind' k stack env
	 `seq`	case nub $ (k : map (\t -> checkType' t stack env) ts) of
		 [k']	-> k'
		 _	-> panic stage
			$  "Kind error in type sum."
			%  "   type:           " % tt
	
	TCon tc
	 -> let k	= tyConKind tc
	    in	checkKind' k stack env 
		 `seq` k
			
	TVar k (UVar v)
	 ->    checkKind' k stack env
	 `seq` case Map.lookup v (envKinds env) of
		Nothing	
		 | envClosed env
		 -> panic stage
			$ "Type variable " % v % " is out of scope.\n"
			
		 | otherwise
		 -> k

		Just k'
		 | k == k'	
		 -> k

		 | otherwise	
		 -> panic stage
		 	$ "Kind error on type variable."
			% "    kind on annot: " % k % "\n"
			% "    does not match environment: " % k % "\n"
		 
	TVar _ (UIndex i)
	 -> let	getTypeIx 0 (x:xs)	= x
		getTypeIx n (x:xs)	= getTypeIx (n-1) xs
		getTypeIx _ []		
			= panic stage
			$ "Debruijn index in type is not bound"
			
	    in	getTypeIx i stack


-- | Lint a Fetter (unfinished)
lintF :: Fetter -> Env -> ()
lintF f env	= ()

-- | Lint some constraints (unfinished)
lintCRS :: Constraints -> Env -> ()
lintCRS crs env	= ()


-- Kind -------------------------------------------------------------------------------------------
-- | Check a kind, returning its superkind.
checkKind :: Kind -> Env -> Super
checkKind kk env
	= checkKind' kk [] env


-- TODO: do the superkind applications.
checkKind' 
	:: Kind 	-- Kind to check.
	-> [Kind] 	-- Stack of kinds of bound type variables, 
			--	referenced by the De'Bruijn type indicies in
			--	the types of witness constructors.
	-> Env
	-> Super
	
checkKind' kk stack env
 = case kk of
 	KNil	-> panic stage $ "checkKind: found a KNil"

	KCon kiCon super
	 -> 	checkKiCon kiCon
	 `seq`	checkSuper super
	 `seq`	super
	
	KFun k1 k2
	 ->	checkKind' k1 stack env
	 `seq`	checkKind' k2 stack env
	 
	KApp k1 t1
	 | KFun k11 k12	<- k1
	 , checkType' t1 stack env == k1
	 -> 	checkKind' k12 stack env
	
	KSum []	-> SProp
	KSum (k:ks)
	 -> 	checkKind' k stack env
	 `seq`	checkKind' (KSum ks) stack env
	
	
-- | Check that a kind is an atomic kind.
checkAtomicKind :: Kind -> ()
checkAtomicKind kk
 = case kk of
	KCon KiConValue   SBox	-> ()
	KCon KiConRegion  SBox	-> ()
	KCon KiConEffect  SBox	-> ()
	KCon KiConClosure SBox	-> ()
	_ -> panic stage $ "Kind " % kk % " is not atomic."	


-- | Check a kind constructor, 
checkKiCon :: KiCon -> ()
checkKiCon kc
 = kc `seq` ()


-- Super ------------------------------------------------------------------------------------------
-- | Check a superkind.
checkSuper :: Super -> ()
checkSuper ss
 = case ss of
	SProp	-> ()
	SBox	-> ()
	SFun k super
	 -> 	checkSuper super
	 `seq`	checkAtomicKind  k
	


{-
-- Var --------------------------------------------------------------------------------------------
-- | Lint a bound value variable.

lintBoundVar :: Var -> Env -> ()
lintBoundVar v env
 = case Map.lookup v (envTypes env) of
 	Nothing  -> panic stage $ "Variable " % v % " is not in scope.\n"
	Just _	 -> ()

-- | Lint a bound type variable.
lintBoundVarT :: Var -> Env -> ()
lintBoundVarT v env
 = case Map.lookup v (envKinds env) of
 	Nothing  -> panic stage $ "Variable " % v % " is not in scope.\n"
	Just _	 -> ()


-- | Check the type of the main function.
--	
lintMainType :: Table -> Type -> LintM ()
lintMainType table tt
	
	-- All witnesses passed to main need to be available at top level.
 	| TContext k t	<- tt
	, Just ks	<- sequence $ map kindOfType $ Map.elems $ tableTypes table
	= if elem k ks
 		then lintMainType table t
		else do	addError $ "Context of main function " % k % " is not available at top level.\n"
			return ()
	
	-- main must have type () -> ()
	| Just (t1, t2, eff, clo)	<- takeTFun tt
	, Just (v1, _, [])		<- takeTData t1
	, Just (v2, _, [])		<- takeTData t2
	, v1 == Var.primTUnit
	, v2 == Var.primTUnit
	= return ()
	
	| otherwise
	= do	addError
			$ "Main function does not have type () -> ().\n"
			% "    T[main] = " % tt	% "\n"
		return ()
-}
