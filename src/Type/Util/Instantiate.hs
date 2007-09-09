
module Type.Util.Instantiate
(
	instantiateT,
	instantiateT_table
)

where

-----
import Util
import qualified Debug.Trace	as Debug

import qualified Data.Map	as Map
import Data.Map			(Map)

-----
import Shared.Error 
import qualified Shared.Var as Var
import Shared.Var (Var)

import Type.Exp
import Type.Pretty
import Type.Plate.Trans

import Type.Util.Bits

-----
stage	= "Type.Instantiate"

instantiateT 
	:: Monad m
	=> (Var -> m Var)
	-> Type -> m Type

instantiateT instF t
 = do
 	(t', _)	<- instantiateT_table instF t
	return t'


instantiateT_table
	:: Monad m
	=> (Var -> m Var)
	-> Type -> m (Type, [Var])


	
instantiateT_table instF t
 = case t of
 	TForall vks x
	 -> do	let vs		= map fst vks
	 	vsI		<- mapM instF vs
		
		let table	= Map.fromList
				$ zip vs vsI
				
		let x'		= transformV (\v -> case Map.lookup v table of
							Nothing	-> v
							Just v'	-> v')
				$ x
				
		return 		(x', vsI)
	
	TSig x
	 -> do	(x', table)	<- instantiateT_table instF x
	 	return 	(TSig x', table)

	TSigExact x
	 -> do	(x', table)	<- instantiateT_table instF x
	 	return	(TSigExact x', table)
		
	_ ->	return (t, [])
	
	
 
 
 
 
 
 




