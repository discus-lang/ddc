
-- | Functions for reducing the context of a type.

module Type.Context
	( reduceContextT )

where

------
import Type.Exp
import Type.Plate
import Type.Util
import Shared.VarPrim


import Util
import qualified Data.Map	as Map
import Data.Map			(Map)

import Debug.Trace
-----


-- | Reduce the context of this type using the provided map of instance definitions.

reduceContextT 
	:: Map Var [Fetter]	-- (class var -> instances) for this class
	-> Type			-- the type to reduce  
	-> Type
	
reduceContextT classInst tt
 = case tt of
 	TFetters fs t	
	 -> let	fs'	= catMaybes
	 		$ map (reduceContextF classInst) fs

	    in	case fs' of
	    		[]	-> t
			_	-> TFetters fs' t
			
	_ 		-> tt
	

reduceContextF 
	:: Map Var [Fetter]	-- (class var -> instances) for each class
	-> Fetter		-- the constraint being used
	-> Maybe Fetter		-- maybe a new constraint

reduceContextF classInstances ff

	-- If there is a matching instance for this class then we can remove the constraint.
 	| FConstraint v ts	<- ff
	, Just instances	<- Map.lookup v classInstances
	, Just inst'		<- find (matchInstance ff) instances
	= Nothing

	-- Purity constraints on bottom effects can be removed.
	| FConstraint v [TBot KEffect]	<- ff
	, v == primPure 
	= Nothing
	
	| otherwise
	= Just ff 	




-- Checks if an class instance supports a certain type.
--	The class instance has to be more polymorphic than the type we want to support.
--
--	This is duplicated in Core.Dictionary for Core.Type
--
matchInstance 
	:: Fetter 	-- the class we want to support
	-> Fetter	-- the class of the instance
	-> Bool

matchInstance cType cInst
	| FConstraint v1 ts1		<- cType
	, FConstraint v2 ts2		<- cInst

	-- check the class is the same
	, v1 == v2
	, length ts1 == length ts2

	-- all the type arguments of the class must unify
	, Just constrs		<- sequence $ zipWith unifyT2 ts1 ts2

	-- any extra constraint from the unification must have 
	--	a var or wildcard for the RHS
	, and 	$ map (\(ta, tb) -> case tb of
				TVar  k v
				 | kindOfType ta == k
			 	 -> True

				TWild k
				 | kindOfType ta == k
				 -> True

				_	-> False)

		$ concat $ constrs
	= True

	| otherwise
	= False
