
-- TODO: finish proper scope checking.
module DDC.Core.Lint.Type
	( checkTypeI
	, checkKindI)
where
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Core.Lint.Base
import DDC.Core.Lint.Env
-- import DDC.Core.Glob
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
	-> (Type, Kind)

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


checkType_trace 
	:: Int -> Type -> Env 
	-> (Type, Kind)

checkType_trace m tt env
 = let n	= m + 1
   in case tt of
	TNil	
	 -> panic stage $ "TNil should not appear in core types.\n"
	
	TForall b k1 t2
	 -> case b of
		BNil	
		 | (k1', w1)	<- checkKindI n k1 env
		 , (t2', k2)	<- checkTypeI n t2 env
		 -> w1 `seq`
			( TForall b k1' t2'
			, k2)
					
		BVar v	
		 | (k1', w1)	<- checkKindI n k1 env
		 , (t2', k2)	<- withKindBound v k1' Nothing env 
				$! checkTypeI n t2
		 -> w1 `seq`
			( TForall b k1' t2'
			, k2)

		BMore v t3
		 | (k1', w1)	<- checkKindI n k1 env
		 , (t3', k3)	<- checkTypeI n t3 env
		 , isEquiv $ equivKK k1' k3
		 , (t2', k2)	<- withKindBound v k1' (Just t3') env
				$! checkTypeI n t2
		 -> w1 `seq`
			( TForall (BMore v t3') k1' t2'
			, k2)
	
	-- TODO: Add fetters to environment.
	TFetters t1 fs
	 | (t1', k1)	<- checkTypeI n t1 env
	 , fs'		<- lintList lintF fs env
	 -> 	( TFetters t1' fs'
		, k1)
	
	-- TODO: Add constraints to environment.
	TConstrain t1 crs
	 | crs'	<- lintCRS crs env
	 , (t1', k1)	<- checkTypeI n t1 env
	 ->	( TConstrain t1' crs'
		, k1)
	
	TApp t1 t2
	 | (t2', k2)	<- checkTypeI n t2 env
	 -> case checkTypeI n t1 env of
		(t1', k1@(KFun k11 k12))
		 | k11 == k2	
		 -> ( TApp t1' t2'
		    , applyKT k1 t2' )	
		
		(t1', k1) 
		 -> panic stage $ vcat
			[ ppr "Kind error in type application."
			, "Cannot apply type:\n" 	%> t2'
			, "of kind:\n"			%> k2
			, "to type:\n" 			%> t1'
			, "of kind:\n" 			%> k1]

	-- For region effect and closure sums, 
	-- all the elements should have the kind attached to the sum.
	-- TODO: Do something better than nub here. 
	--       If we only care about these three kinds then we could define an ordering.
	TSum k ts
	 | isRegionKind k || isEffectKind k || isClosureKind k
	 , (k',  w)	<- checkKindI n k env
	 , (ts', ks)	<- unzip $ map (\t -> checkTypeI n t env) ts
	 -> w `seq`
	    case nub $ (k' : ks) of
		 [kk]	-> (TSum k' ts', kk)
		 kks	-> panic stage $ vcat
			[ ppr "Kind error in type sum."
			, "   type:           " % tt
			, "   kinds in sum:   " % kks ]

	 -- For witness sums, the kind of the sum is a KSum which contains
	 -- the kind of all the witnesses.
	 | otherwise
	 , (ts', ks)	<- unzip $ map (\t -> checkTypeI n t env) ts
	 , k'		<- makeKSum ks
	 -> if isEquiv $ equivKK k k'
		 then  (TSum k' ts', k')
		 else  panic stage $ vcat
			[ ppr "Kind error in type sum."
			, "    type:           " % tt
			, "    kind of sum:    " % k
			, "    kind of elems:  " % k']
	
	-- Type constructors.
	TCon tc
	 | k		<- tyConKind tc
	 , (k', w)	<- checkKindI n k env 
	 -> w `seq`
		( TCon tc
		, k')
			
	-- Type variables.
	TVar k (UMore v TNil)
	 -> panic stage $ ppr "checkType: no bound on UMore"

	TVar _ UClass{}
	 -> panic stage $ ppr "checkType: Found a UClass. These shouldn't show up in the core IR."
	
	TVar k b
	 | (k', w)	<- checkKindI n k env
	 , Just v	<- takeVarOfBound b
	 -> w `seq`
	    case Map.lookup v (envKindBounds env) of
		Nothing	
--		  | not $ envClosed env
		  -> (TVar k' b, k')

{-		  -- Region variables may be bound at top level.
		  | isRegionKind k
		  ,    varIsBoundAtTopLevelInGlob (envHeaderGlob env) v
		    || varIsBoundAtTopLevelInGlob (envModuleGlob env) v
		  -> (TVar k b, k)

		  | otherwise
		  -> panic stage $  vcat
			[ "Type variable " % v % " is out of scope."
			, "During: "	% envCaller env]
-}
		Just (k'', _)	
		 | isEquiv $ equivKK k' k''	
		 -> (TVar k' b, k')

		 | otherwise	
		 -> panic stage
		 	$ "Kind error on type variable."
			% "    kind on annot: " % k % "\n"
			% "    does not match environment: " % k % "\n"
		 

	-- TODO: check index is in scope.
	TVar k (UIndex i)
	 | (k', w)	<- checkKindI n k env
	 -> w `seq`
		( TVar k' (UIndex i)
		, k')
	
	TError{}
	 -> panic stage $ ppr "checkType: no match for TError"


-- | Lint a Fetter (unfinished)
lintF :: Fetter -> Env -> Fetter
lintF f _	= f

-- | Lint some constraints (unfinished)
lintCRS :: Constraints -> Env -> Constraints
lintCRS crs _	= crs


-- Kind -------------------------------------------------------------------------------------------
-- | Check a kind, returning its superkind.
checkKindI :: Int -> Kind -> Env -> (Kind, Super)
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
 	KNil	
  	 -> panic stage $ "checkKind: found a KNil"

	KCon kiCon w
	 | checkKiCon  kiCon
	 , w'	<- checkSuperI n w env
	 -> 	( KCon kiCon w'
		, w')

	 | otherwise
	 -> panic stage $ vcat
		[ ppr "Malformed kind constructor"
		, "With kiCon:    " % kiCon
		, "and superkind: " % w ]
		
	KFun k1 k2
	 | (k1', w1)	<- checkKindI n k1 env
	 , (k2', w2)	<- checkKindI n k2 env
	 -> w1 `seq`
		( KFun k1' k2'
		, w2)
	 
	KApp k1 t2
	 | (k1', SFun k11 w12)	<- checkKindI n k1 env 
	 , (t2', k2)		<- checkTypeI n t2 env
	 , isEquiv $ equivKK k11 k2
	 -> 	( KApp k1' t2'
		, w12 )
	
 	 | otherwise
 	 -> panic stage $ vcat
		[ ppr "Error in type/kind application"
		, "Cannot apply type: " % t2
		, "to kind:           " % k1
		, "in kind:           " % kk]
	
	KSum kks
	 -> let	checkKinds !ks' []	
		 = (KSum (reverse ks'), SProp)

		checkKinds !ks' (k:ks)	
		 = let 	(!k', w) = checkKindI n k env
		   in	w `seq` checkKinds (k' : ks') ks
		
	    in	checkKinds [] kks
		
	
-- | Check a kind constructor.
checkKiCon :: KiCon -> Bool
checkKiCon kc
 = kc `seq` True


-- Super ------------------------------------------------------------------------------------------
-- | Check a superkind.
checkSuperI :: Int -> Super -> Env -> Super
checkSuperI n ss env
 = case ss of
	SProp		-> ss
	SBox		-> ss
	SFun k1 w2
	 | (!k1', w1)	<- checkKindI  (n+1) k1 env
	 , !w2'		<- checkSuperI (n+1) w2 env
	 -> w1 `seq` 
		SFun k1' w2'
	
