-- Type instantiation.
--
module Type.Util.Instantiate
	( instantiateT
	, instantiateT_table )
where

-----
import Util
import qualified Debug.Trace	as Debug

import qualified Data.Map	as Map
import Data.Map			(Map)

-----
import qualified Shared.Var as Var
import Shared.Var (Var)

import Type.Exp
import Type.Pretty
import Type.Plate.Trans

import Type.Util.Bits

-----
-- stage	= "Type.Instantiate"

-- | Instantiate a type scheme using the provided gn to create
--	the new variables.
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

instantiateT_table instF t
 = case t of
 	TForall vks x
	 -> do	-- build a table mapping each of the forall bound variables
		--	to a new instance variable.
	 	let vs		= map fst vks
	 	Just vsI	<- liftM sequence $ mapM instF vs
		let table	= Map.fromList $ zip vs vsI
				
		-- substitute instance variables into the body of the type.
		let x'		= transformV (\v -> case Map.lookup v table of
							Nothing	-> v
							Just v'	-> v')
				$ x
				
		return 		(x', vsI)
	
	_ ->	return (t, [])
	
