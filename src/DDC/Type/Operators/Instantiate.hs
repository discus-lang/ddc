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


-- | Instantiate a type with these arguments.
--
--   TODO: Check argument kinds match.
instantiateT :: Type -> [Type] -> Type
instantiateT tScheme tsArgs
 = let	(bks, tBody)	= takeTForall tScheme
	
	Just vsQuant 	= sequence $ map (takeVarOfBind . fst) bks
	table		= Map.fromList $ zip vsQuant tsArgs
	
   in	subVT_everywhere table tBody



-- | Instantiate a type scheme, using the provided function to create
--	the new variables, and also return the new instance vars created.
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

