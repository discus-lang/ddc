-- Type instantiation.
--
module Type.Util.Instantiate
	( instantiateT
	, instantiateT_table )
where
import Type.Plate.Trans
import Util
import DDC.Var
import DDC.Type.Exp
import DDC.Type.Compounds
import Type.Pretty		()
import qualified Data.Map	as Map


-- | Instantiate a type scheme using the provided gn to create
--   the new variables.
--   TODO: Make this a a pure function, and require the caller to supply enough
--	   fresh varaibles to perform the instantiation.
--         Instantiation should be just type-type application.	
instantiateT 
	:: Monad m
	=> (Var -> m (Maybe Var))
	-> Type -> m Type

instantiateT instF t
 = do
 	(t', _)	<- instantiateT_table instF t
	return t'


-- | Instantiate a type scheme, using the provided function to create
--	the new variables, and also return the new instance vars created.
instantiateT_table
	:: Monad m
	=> (Var -> m (Maybe Var))	-- ^ fn to instantiate a variable.
	-> Type 			-- ^ type to instantiate
	-> m ( Type			-- instantiated type
	     , [Var])			-- list of instance variables, one for 
	     				--	each of the foralls at the front of the scheme

instantiateT_table instF tt
 = case tt of
	TForall BNil _ _
	 ->	return (tt, [])
	
 	TForall b k tBody
	 -> do	-- split of the quantifier so we can instantiate all the vars at once
	 	let (bks, tBody) = takeTForall tt
	 
		-- build a table mapping each of the forall bound variables
		--	to a new instance variable.
	 	let  Just vs	= sequence $ map (takeVarOfBind . fst) bks
	 	Just vsI	<- liftM sequence $ mapM instF vs
		let table	= Map.fromList $ zip vs vsI
				
		-- substitute instance variables into the body of the type.
		let tBody'	= transformV 
					(\v -> case Map.lookup v table of
						Nothing	-> v
						Just v'	-> v')
					tBody
				
		return 		(tBody', vsI)
	
	_ ->	return (tt, [])
	
