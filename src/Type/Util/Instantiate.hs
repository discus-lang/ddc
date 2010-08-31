-- Type instantiation.
--
module Type.Util.Instantiate
	( instantiateType
	, instantiateTypeWithFreshVars)
where
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Transform
import DDC.Type.Substitute
import DDC.Type.Pretty		()
import DDC.Var
import qualified Data.Map	as Map

-- | Instantiate a type with these arguments.
instantiateType :: Type -> [Type] -> Type
instantiateType tScheme tsArgs
 = let	(bks, tBody)	= takeTForall tScheme
	
	Just vsQuant 	= sequence $ map (takeVarOfBind . fst) bks
	table		= Map.fromList $ zip vsQuant tsArgs
	
   in	subVT_everywhere table tBody



-- | Instantiate a type scheme, using the provided function to create
--	the new variables, and also return the new instance vars created.
instantiateTypeWithFreshVars
	:: Monad m
	=> (Var -> m Var)	-- ^ Function to instantiate each variable.
	-> Type 		-- ^ Type scheme to instantiate.
	-> m ( Type		--   Instantiated type.
	     , [Var])		--   List of instance varaibles, one for each outer
				--   forall quantifier in the original scheme.
	
instantiateTypeWithFreshVars instVar tt
 = case tt of
 	TForall b k tBody
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

