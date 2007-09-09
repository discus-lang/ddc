
module Type.Util.Erase
{-	( eraseCloT
	, eraseFettersT
	, eraseEffectsT 
	, eraseNodes 
	, stripNodeFettersT)
-}
where

import Util
import Type.Exp
import Type.Util.Bits

import Type.Plate.Trans


{-
-----------------------
-- eraseCloT 
--	Erase environment information from types
--
eraseCloT ::	Type	-> Type
eraseCloT	t
 	= transformT 
		(\t -> case t of
			TFun t1 t2 eff clo	-> TFun t1 t2 eff CNil
			_			-> t)
	$ t
	

-----------------------
-- eraseFettersT
--	Erase fetters from types
--
eraseFettersT :: Type -> Type
eraseFettersT	 t
	= transformT 
		(\x -> case x of
			TFetters fs x'	-> x'
			_		-> x)
	$ t	


-----------------------
-- eraseEffectsT
--	Erase effect fetters from a type.
--
eraseEffectsT :: Type -> Type
eraseEffectsT t
 	= transZ transTableId 
		{ transT = \x -> case x of
				TFetters fs x' 	-> return $ TFetters (filter (not.isFEffect) fs) x'
				_		-> return $ x
				
		, transE = \x -> case x of
				EFetters fs e	-> return $ e
				_		-> return $ x }
	$ t
	
			

-----------------------
-- eraseNodesT
--
-- eraseNodesT :: Type -> Type
eraseNodes tt
	= transZ transTableId
		{ transT = \x -> case x of
				TNode cid t	-> return t
				_		-> return x
				
		, transE = \x -> case x of
				ENode cid t	-> return $ EClass cid
				_		-> return x
				
		, transC = \x -> case x of
				CNode cid t	-> return $ CClass cid
				_		-> return x }
	$ tt
			

-----------------------
-- stripNodeFetters
--
stripNodeFettersT :: Type -> [Fetter]
stripNodeFettersT tt
 	= execState
		(transZM transTableId
			{ transE
				= \x -> case x of
					ENode cid e	
					 -> do	modify (\s -> FEffect (EClass cid) e : s)
					 	return x
					
					_ -> 	return x
					
			, transC
				= \x -> case x of
					CNode cid c	
					 -> do	modify (\s -> FClosure (CClass cid) c : s)
					 	return x
						
					_ -> 	return x }
			tt)
		[]


-}


