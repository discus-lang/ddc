
module DDC.Core.Lint.Type
	( checkTypeI
	, checkKindI)
where
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Core.Lint.Base
import DDC.Core.Lint.Env
import DDC.Type
import Data.List
import qualified Data.Map	as Map

stage	= "DDC.Core.Lint.Type"	

-- Type -------------------------------------------------------------------------------------------
-- | Check a type expression, returning its kind.
--	This does a complete check of the entire structure, so is not fast.
--	If you just want to quickly get the kind of a type then use kindOfType instead.
--
checkTypeI
	:: Int		-- Indent level, for debugging. 
	-> Type 	-- Type to check.
	-> Env 		-- Type and Kind Environment.
	-> Kind

checkTypeI n tt env
 = if debugType
    then let !kind
		= trace (setColumn (n*indenting) % vcat
			[ ppr (replicate 70 '-') <> "Type" <> n
			, ppr tt ])
		$ checkType_trace n tt env
	 in trace 
		(setColumn (n*indenting) % vcat 
		[ "kind:   " 	% kind
		, ppr tt
		, "--" <> "Type" <> n <> ppr (replicate 70 '-')])
		kind
   else	checkType_trace n tt env


-- NOTE: All kinds returned by this function must be checked by checkKind at some point.
checkType_trace :: Int -> Type -> Env -> Kind
checkType_trace m tt env
 = let n	= m + 1
   in case tt of
	TNil	
	 -> panic stage $ "TNil should not appear in core types.\n"
	
	TForall b k1 t2
	 -> case b of
		BNil	
		 ->	checkKindI n k1 env	
		 `seq`	checkTypeI n t2 env
		
		BVar v	
		 -> 	checkKindI n k1 env
		 `seq`	withKind v k1 env	$!
			checkTypeI n t2
			
		BMore v t1
		 -> 	checkKindI n k1 env
		 `seq`	withKind  v k1 env 	$! \env'  ->
		   	withBound v t1 env' 	$!
		   	checkTypeI n t2
	
	-- TODO: Add fetters to environment.
	TFetters t1 fs
	 ->	lintList lintF fs env
	 `seq`	checkTypeI n t1 env
	
	-- TODO: Add constraints to environment.
	TConstrain t1 crs
	 ->	lintCRS crs env
	 `seq`	checkTypeI n t1 env
	
	-- TODO: We want a faster way of doing the substitution in
	--       in the kind/type application.
	TApp t1 t2
	 -> let k2	= checkTypeI n t2 env
	    in case checkTypeI n t1 env of
		k1@(KFun k11 k12)
		 | k11 == k2	-> uncheckedApplyKT k1 t2
		
		k1 -> panic stage
		 $ vcat [ ppr "Kind error in type application."
			, "Cannot apply type:\n" 	%> t2
			, "of kind:\n"			%> k2
			, "to type:\n" 			%> t1
			, "of kind:\n" 			%> k1]
		
	-- TODO: nub is horrible.
	TSum k ts
	 -> 	checkKindI n k  env
	 `seq`	case nub $ (k : map (\t -> checkTypeI n t env) ts) of
		 [k']	-> k'
		 _	-> panic stage
			$  "Kind error in type sum."
			%  "   type:           " % tt
	
	-- Type constructors.
	TCon tc
	 -> let k	= tyConKind tc
	    in	checkKindI n k env 
		 `seq` k
		
	-- Type variables.
	TVar _ (UMore{})	-> panic stage $ ppr "checkType: TVar UMore not finished"
	TVar _ (UClass{})	-> panic stage $ ppr "checkType: TVar UClass not finished"
	
	TVar k (UVar v)
	 ->    checkKindI n k env
	 `seq` case Map.lookup v (envKinds env) of
		Nothing	
		 | envClosed env
		 -> panic stage	
			$ "Type variable " % v % " is out of scope.\n"
			
		 | otherwise		-> k

		Just k'	 | k == k'	-> k

		 | otherwise	
		 -> panic stage
		 	$ "Kind error on type variable."
			% "    kind on annot: " % k % "\n"
			% "    does not match environment: " % k % "\n"
		 
	-- TODO: check index is in scope.
	TVar k (UIndex i)
	 -> k
	
	TError{}
	 -> panic stage $ ppr "checkType: no match for TError"

-- | Lint a Fetter (unfinished)
lintF :: Fetter -> Env -> ()
lintF _ _	= ()

-- | Lint some constraints (unfinished)
lintCRS :: Constraints -> Env -> ()
lintCRS _ _	= ()


-- Kind -------------------------------------------------------------------------------------------
-- | Check a kind, returning its superkind.
-- TODO: do the superkind applications.
checkKindI :: Int -> Kind -> Env -> Super
checkKindI n kk env
 = if debugKind
    then let !super
		= trace (setColumn (n*indenting) % vcat
			[ ppr (replicate 70 '-') <> "Kind" <> n
			, ppr kk])
		$ checkKind_trace n kk env
	 in trace 
		(setColumn (n*indenting) % vcat 
		[ "super:   " 	% super
		, ppr kk
		, "--" <> "Kind" <> n <> ppr (replicate 70 '-')])
		super
   else	checkKind_trace n kk env


checkKind_trace m kk env
 = let n	= m + 1
   in case kk of
 	KNil	-> panic stage $ "checkKind: found a KNil"

	KCon kiCon super
	 | checkKiCon  kiCon
	 ->    checkSuperI n super env
	 `seq` super

	 | otherwise
	 -> panic stage $ vcat
		[ ppr "Malformed kind constructor"
		, "With kiCon:    " % kiCon
		, "and superkind: " % super ]
		
	KFun k1 k2
	 ->	checkKindI n k1 env
	 `seq`	checkKindI n k2 env
	 
	KApp k1 t1
	 |  SFun k11 w	<- checkKindI n k1 env 
	 ,  checkTypeI n t1 env == k11
	 -> checkSuperI n w env `seq` w
	
 	 | otherwise
 	 -> panic stage $ vcat
		[ ppr "Error in type/kind application"
		, "Cannot apply type: " % t1
		, "to kind:           " % k1
		, "in kind:           " % kk]
	
	KSum []	-> SProp
	KSum (k:ks)
	 -> 	checkKindI n k env
	 `seq`	checkKindI n (KSum ks) env
	
	
-- | Check that a kind is an atomic kind.
{-
checkAtomicKind :: Kind -> Bool
checkAtomicKind kk
 = case kk of
	KCon KiConValue   SBox	-> True
	KCon KiConRegion  SBox	-> True
	KCon KiConEffect  SBox	-> True
	KCon KiConClosure SBox	-> True
	_ -> freakout stage ("Kind " % kk % " is not atomic.") False
-}

-- | Check a kind constructor, 
checkKiCon :: KiCon -> Bool
checkKiCon kc
 = kc `seq` True


-- Super ------------------------------------------------------------------------------------------
-- | Check a superkind.
checkSuperI :: Int -> Super -> Env -> ()
checkSuperI n ss env
 = case ss of
	SProp		-> ()
	SBox		-> ()
	SFun k super
	 ->    checkKindI  (n+1) k env
	 `seq` checkSuperI (n+1) super env
	



