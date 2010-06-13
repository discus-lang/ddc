
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
checkTypeI :: Int -> Type -> Env -> Kind
checkTypeI n tt env 
	= checkType' n tt [] env 

-- | Check a type expression, returning its kind.
--	This does a complete check of the entire structure, so is not fast.
--	If you just want to quickly get the kind of a type then use kindOfType instead.
--
checkType' 
	:: Int		-- Indent level, for debugging. 
	-> Type 	-- Type to check.
	-> [Type] 	-- Stack of types bound via (kind type) application 
			--	referenced by the De'Bruijn type indicies in
			--	the types of witness constructors.
	-> Env 		-- Type and Kind Environment.
	-> Kind

checkType' n tt stack env
 = if debugType
    then let !kind
		= trace (setColumn (n*indenting) % vcat
			[ ppr (replicate 70 '-') <> "Type" <> n
			, ppr tt ])
		$ checkType_trace n tt stack env
	 in trace 
		(setColumn (n*indenting) % vcat 
		[ "kind:   " 	% kind
		, ppr tt
		, "--" <> "Type" <> n <> ppr (replicate 70 '-')])
		kind
   else	checkType_trace n tt stack env


-- NOTE: All kinds returned by this function must be checked by checkKind at some point.
checkType_trace m tt stack env
 = let n	= m + 1
   in case tt of
	TNil	-> panic stage $ "TNil should not appear in core types.\n"
	
	TForall b k1 t2
	 -> case b of
		BNil	
		 ->	checkKind' n k1 stack env	
		 `seq`	checkType' n t2 stack env
		
		BVar v	
		 -> 	checkKind' n k1 stack env
		 `seq`	withKind v k1 env	$!
			checkType' n t2 stack
			
		BMore v t1
		 -> 	checkKind' n k1 stack env
		 `seq`	withKind  v k1 env 	$! \env'  ->
		   	withBound v t1 env' 	$!
		   	checkType' n t2 stack
	
	-- TODO: Add fetters to environment.
	TFetters t1 fs
	 ->	lintList lintF fs env
	 `seq`	checkType' n t1 stack env
	
	-- TODO: Add constraints to environment.
	TConstrain t1 crs
	 ->	lintCRS crs env
	 `seq`	checkType' n t1 stack env
	
	-- TODO: Just use one panic case here.
	TApp t1 t2
	 -> case checkType' n t1 stack env of
		KFun k11 k12
		 | k11 == checkType' n t2 stack env 
		 -> 	  checkKind' n k12 stack env
		 `seq`	k12
		
		 | otherwise
		 -> panic stage 
		 $ vcat	[ ppr "Kind error in type application."
			, ppr "    kind:           whatever" -- checkType' n' t2 stack env	
			, "    does not match: " % k11 
			, "    in type:        " % tt ]

		k1 -> panic stage
		 $ vcat [ ppr "Kind error in type application."
			, "    cannot apply type: " % t2
			, "    to type:           " % t1
			, "    which has kind:    " % k1]
		
				
	TSum k ts
	 -> 	checkKind' n k stack env
	 `seq`	case nub $ (k : map (\t -> checkType' n t stack env) ts) of
		 [k']	-> k'
		 _	-> panic stage
			$  "Kind error in type sum."
			%  "   type:           " % tt
	
	TCon tc
	 -> let k	= tyConKind tc
	    in	checkKind' n k stack env 
		 `seq` k
		
	TVar _ (UMore{})	-> panic stage $ ppr "checkType: TVar UMore not finished"
	TVar _ (UClass{})	-> panic stage $ ppr "checkType: TVar UClass not finished"
	
			
	TVar k (UVar v)
	 ->    checkKind' n k stack env
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
		 
	-- TODO: check index is in scope.
	TVar k (UIndex i)
	 -> k
	
{-	 -> let	getTypeIx 0 (x:_)	= x
		getTypeIx z (_:xs)	= getTypeIx (z-1) xs
		getTypeIx _ []		
			= panic stage $ vcat
			[ ppr "Debruijn index in type is not bound"
			, "For index:\n" 	%> i
			, "stack was:\n" 	%> stack]
			
	    in	getTypeIx i stack
-}

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
checkKindI :: Int -> Kind -> Env -> Super
checkKindI n kk env
	= checkKind' n kk [] env


-- TODO: do the superkind applications.
checkKind' 
	:: Int
	-> Kind 	-- Kind to check.
	-> [Type] 	-- Stack of type bound by (kind type) application
			-- 	These are referenced by the De'Bruijn type indicies in
			--	the types of witness constructors.
	-> Env
	-> Super

checkKind' n kk stack env
 = if debugKind
    then let !super
		= trace (setColumn (n*indenting) % vcat
			[ ppr (replicate 70 '-') <> "Kind" <> n
			, ppr kk 
			, "stack = " % stack])
		$ checkKind_trace n kk stack env
	 in trace 
		(setColumn (n*indenting) % vcat 
		[ "super:   " 	% super
		, ppr kk
		, "--" <> "Kind" <> n <> ppr (replicate 70 '-')])
		super
   else	checkKind_trace n kk stack env


checkKind_trace m kk stack env
 = let n	= m + 1
   in case kk of
 	KNil	-> panic stage $ "checkKind: found a KNil"

	KCon kiCon super
	 -> 	checkKiCon kiCon
	 `seq`	checkSuper super
	 `seq`	super
	
	KFun k1 k2
	 ->	checkKind' n k1 stack env
	 `seq`	checkKind' n k2 stack env
	 
	KApp k1 t1
	 | SFun k11 w	<- checkKind' n k1 stack env 
	 , checkType' n t1 stack env == k11
	 -> checkSuper w `seq` w

 	 | otherwise
 	 -> panic stage $ vcat
		[ ppr "Error in type/kind application"
		, "Cannot apply type: " % t1
		, "to kind:           " % k1
		, "in kind:           " % kk]
	
	KSum []	-> SProp
	KSum (k:ks)
	 -> 	checkKind' n k stack env
	 `seq`	checkKind' n (KSum ks) stack env
	
	
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
	


