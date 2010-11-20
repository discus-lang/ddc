{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Type.Operators.Instantiate
	( instantiateT
	, instantiateWithFreshVarsT)
where
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Transform
import DDC.Type.Operators.Substitute
import DDC.Type.Pretty		()
import DDC.Var
import qualified Data.Map	as Map


-- | Instantiate a type with a list of type arguments.
--   If there are more args than foralls at the front of the type then `Nothing`.
--   We don't also check that the kinds match up along the way.
--
instantiateT :: Type -> [Type] -> Maybe Type
instantiateT tt ts
	= instantiateT' Map.empty tt ts
	
instantiateT' sub t1 []	
	= Just (subTT_noLoops sub t1)

instantiateT' sub t1 (t2 : ts)
	| TForall (BVar v) k11 t12	<- t1
	, t11 <- TVar k11 $ UVar v
	= if t11 /= t2
		then instantiateT' (Map.insert t11 t2 sub) t12 ts
		else instantiateT' sub t12 ts
	
	| TForall (BMore v tMore) k11 t12 <- t1
	, t11 <- TVar k11 $ UMore v tMore
	= if t11 /= t2
		then instantiateT' (Map.insert t11 t2 sub) t12 ts
		else instantiateT' sub t12 ts
	
	| TForall BNil _ t12		<- t1
	= instantiateT' sub t12 ts
	
	| otherwise
	= Nothing	



-- | Instantiate a type scheme, using the provided function to create
--	the new variables, and also return the new instance vars created.
--
--   TODO: this don't handle contexts at the front of types.
--
instantiateWithFreshVarsT
	:: Monad m
	=> (Var -> m Var)	-- ^ Function to instantiate each variable.
	-> Type 		-- ^ Type scheme to instantiate.
	-> m ( Type		--   Instantiated type.
	     , [Var])		--   List of instance varaibles, one for each outer
				--   forall quantifier in the original scheme.
	
instantiateWithFreshVarsT instVar tt
 = case tt of
 	TForall{}
	 -> do	-- split of the quantifier so we can instantiate all the vars at once
	 	let (bks, tBody) = takeTForall tt
	 
		-- build a table mapping each of the forall bound variables
		--	to a new instance variable.
	 	let Just vsQuant = sequence $ map (takeVarOfBind . fst) bks
	 	vsInst		<- mapM instVar vsQuant
		let table	= Map.fromList $ zip vsQuant vsInst
				
		-- substitute instance variables into the body of the type.
		let tBody'	= transformV 
					(\v -> case Map.lookup v table of
						Nothing	-> v
						Just v'	-> v')
					tBody
				
		return 		(tBody', vsInst)
	
	_ ->	return (tt, [])

