
-- | Check for lint in the core program.
--   TODO: Do full type checking.
--         Make Core.Reconstruct add type annots, but Core.Lint just check them.
--	   Check syntactic soundness of witnesses.
--	   Check for type vars that are out of scope
--
module Core.Lint
	( lintGlob
	, checkType
	, Env	(..))
where
import Core.Glob
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Type
import DDC.Var
import Data.List
import qualified Data.Map	as Map
import Data.Map			(Map)

stage	= "Core.Lint"

-- Glob -------------------------------------------------------------------------------------------
lintGlob 
	:: Glob 	-- the tree to check
	-> ()

lintGlob glob = ()

{- 
FIXME

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


-- Exp --------------------------------------------------------------------------------------------
lintX :: Exp -> Table -> ()
lintX xx tt
 = case xx of
	XLam v k x
	 -> withKind v k env
	  $ \env' -> lintX env' x
		
	XAPP x t
	 ->    lintX env x 
	 `seq` lintT env t
	 `seq` ()
		
-}
-- Type -------------------------------------------------------------------------------------------
-- | Check a type expression, returning its kind.
--	This does a complete check of the entire structure, so is not fast.
--	If you just want to quickly get the kind of a type then use kindOfType instead.
--
checkType 
	:: Type 	-- Type to check.
	-> [Kind] 	-- Stack of kinds of bound type variables, 
			--	referenced by the De'Bruijn type indicies in
			--	the types of witness constructors.
	-> Env 		-- Type and Kind Environment.
	-> Kind

-- NOTE: All kinds returned by this function must be checked by checkKind at some point.
checkType tt stack env
 = case tt of
	TNil	-> panic stage $ "TNil should not appear in core types.\n"
	
	TForall b k1 t2
	 -> case b of
		BVar v	
		 -> 	checkKind k1 stack env
		 `seq`	withKind v k1 env	$!
			checkType t2 (k1 : stack)
			
		BMore v t1
		 -> 	checkKind k1 stack env
		 `seq`	withKind  v k1 env 	$! \env'  ->
		   	withBound v t1 env' 	$!
		   	checkType t2 (k1 : stack)
	
	-- TODO: Add fetters to environment.
	TFetters t1 fs
	 ->	lintList lintF fs env
	 `seq`	checkType t1 stack env
	
	-- TODO: Add constraints to environment.
	TConstrain t1 crs
	 ->	lintCRS crs env
	 `seq`	checkType t1 stack env
	
	TApp t1 t2
	 -> case checkType t1 stack env of
		KFun k11 k12
		 | k11 == checkType t2 stack env 
		 -> 	 checkKind k12 stack env
		 `seq`	k12
		
		 | otherwise
		 -> panic stage 
		 $ vcat	[ ppr "Kind error in type application"
			, "    kind:           " % checkType t2 stack env	
			, "    does not match: " % k11 
			, "    in type:        " % tt ]
				
	TSum k ts
	 -> 	checkKind k stack env
	 `seq`	case nub $ (k : map (\t -> checkType t stack env) ts) of
		 [k']	-> k'
		 _	-> panic stage
			$  "Kind error in type sum"
			%  "   type:           " % tt
	
	TCon tyCon
	 -> case tyCon of
		TyConFun 	
		 -> KFun kValue kValue

		TyConData v k	
		 -> 	checkKind k stack env
		 `seq`	k
	
		TyConWitness v k
		 -> 	checkKind k stack env
		 `seq`	k
		
	TVar k (UVar v)
	 ->    checkKind k stack env
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

{-						
	
	TEffect v ts
	 ->	lintBoundVarT v env
	 `seq`	lintList lintT ts env
	 `seq`	()
	
	TFree v t
	 -> 	lintBoundVar v env
	 `seq`	lintT t env
	 `seq`	()
	
	TDanger t1 t2
	 -> 	lintT t1 env
	 `seq`	lintT t2 env
	 `seq`	()
	
	TElaborate{}	-> panic stage	$ "TElaborate should not appear in core types."
	TClass{}	-> panic stage	$ "TClass should not appear in core types."
	TError{}	-> panic stage	$ "TError should not appear in core types."
	
	TVarMore k v t
	 -> 	lintK k env
	 `seq`	lintBoundVarT v env
	 `seq`	lintT t env
	 `seq`	(when (kindOfType t /= Just k)
			$ panic stage $ "TVarMore constraint has wrong kind\n" % tt)
	 `seq`	()
	
	TWitJoin ts
	 ->	lintList lintT ts env 
	 `seq`	()
-}

-- | Lint a Fetter (unfinished)
lintF :: Fetter -> Env -> ()
lintF f env	= ()

-- | Lint some constraints (unfinished)
lintCRS :: Constraints -> Env -> ()
lintCRS crs env	= ()

-- | Lint a type constructor (unfinished)
{-
lintTyCon :: TyCon -> ()
lintTyCon tc	= ()
-}


-- Kind -------------------------------------------------------------------------------------------

-- TODO: Make this return the superkind.
checkKind 
	:: Kind 	-- Kind to check.
	-> [Kind] 	-- Stack of kinds of bound type variables, 
			--	referenced by the De'Bruijn type indicies in
			--	the types of witness constructors.
	-> Env
	-> Super
	

checkKind kk stack env
 = case kk of
 	KNil	-> panic stage $ "lintK: found a KNil"

	KCon kiCon super
	 -> 	lintKiCon kiCon
	 `seq`	lintSuper super
	 `seq`	super
	
	KFun k1 k2
	 ->	checkKind k1 stack env
	 `seq`	checkKind k2 stack env
	 
	KApp k1 t1
	 | KFun k11 k12	<- k1
	 , checkType t1 stack env == k1
	 -> 	checkKind k12 stack env
	
	KSum []	-> SProp
	KSum (k:ks)
	 -> 	checkKind k stack env
	 `seq`	checkKind (KSum ks) stack env
	

lintSuper :: Super -> ()
lintSuper ss	= undefined			

lintKiCon 	= undefined


-- Var --------------------------------------------------------------------------------------------
-- | Lint a bound value variable.
{-
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
-}

-- Utils ------------------------------------------------------------------------------------------
-- | Check for lint in some list of things.
lintList ::  (a -> Env -> b) -> [a] -> Env -> ()
lintList lintFun xx env
 = case xx of
	[]		-> ()
	(x:xs)		
		->    lintFun  x env
		`seq` lintList lintFun xs env
		`seq` ()

-- Env --------------------------------------------------------------------------------------------
-- | A table of type and kind bindings.
data Env

	= Env
	{ -- | Whether the thing we're checking is supposed to be closed.
	  envClosed		:: Bool
{-
	  --  The header glob, for getting top-level types and kinds.
	, envHeaderGlob		:: Glob

	  --  The core glob, for getting top-level types and kinds.
	, envModuleGlob		:: Glob

	  --  Types of value variables that are in scope at the current point.
	, envTypes		:: Map Var Type
-}
	  -- | Kinds of type variables that are in scope at the current point.
	, envKinds		:: Map Var Kind }
{-
envInit	cgHeader cgModule
	= Env
	{ envClosed		= False
	, envHeaderGlob		= cgHeader
	, envModuleGlob		= cgModule
	, envTypes		= Map.empty
	, envKinds		= Map.empty }
-}

-- | Run a lint computation with an extra type in the environment.
{-
withType :: Var -> Type -> Env -> (Env -> a) -> a
withType v t env fun
 = let	addVT Nothing	= Just t
	addVT Just{}	= panic stage $ "withVarType: type for " % v % " already present"
   in	fun $ env { envTypes = Map.alter addVT v (envTypes env) }
-}

withBound :: Var -> Type -> Env -> (Env -> a) -> a
withBound = undefined

-- | Run a lint computation with an extra kind in the environment.

withKind :: Var -> Kind -> Env -> (Env -> a) -> a
withKind v k env fun
 = let	addVK Nothing	= Just k
	addVK Just{}	= panic stage $ "withVarKind: kind for " % v % " already present"
   in	fun $ env { envKinds = Map.alter addVK v (envKinds env) }

{-
-- | Get a kind from the environment.
getKind :: Var -> Env -> Kind
getKind v env	
  = case Map.lookup v (envKinds env) of
	Nothing	-> panic stage $ "getKind: not in scope " % v
	Just k	-> k


when :: Bool -> () -> ()
{-# INLINE when #-}
when b x
 = case b of
	True	-> x
	False	-> ()
-}
