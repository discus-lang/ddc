
-- | Functions for reducing the context of a type.

module Type.Context
	( reduceContextT 
	, matchInstance )

where
import Type.State
import Type.Exp
import Type.Plate
import Type.Util
import Type.Crush.Fetter	(crushFetter)
import Type.Effect.MaskLocal	(visibleRsT)
import Shared.VarPrim
import Shared.Error

import Util
import Debug.Trace
import qualified Data.Set	as Set
import qualified Data.Map	as Map

-----
-- stage	= "Type.Context"


-- | Reduce the context of this type using the provided map of instance definitions.
reduceContextT 
	:: Map Var [Fetter]	-- (class var -> instances) for this class
	-> Type			-- the type to reduce  
	-> SquidM Type
	
reduceContextT classInst tt
 = case tt of
 	TFetters tShape fs
	 -> do	fs'	<- liftM concat
	 		$ mapM (reduceContextF (flattenT tt) classInst) fs

	   	case fs' of
	    		[]	-> return tShape
			_	-> return $ TFetters tShape fs'
			
	_ 		-> return tt
	

reduceContextF 
	:: Type			-- the shape of the type
	-> Map Var [Fetter]	-- (class var -> instances) for each class
	-> Fetter		-- the constraint being used
	-> SquidM [Fetter]	-- maybe some new constraints

reduceContextF tShape classInstances ff

	-- We can remove Const, Lazy, Mutable, Direct context if they are on regions which are not present
	--	in the shape of the type.
	--
	--	These regions are local to the function and a local region binding will be introduced
	--	which will create the required witnesses.
	--	
	| FConstraint v [tR@(TVar kR r)]	<- ff
	, kR	== kRegion
	, elem v [primConst, primMutable, primMutable, primDirect]
	, not $ Set.member tR $ visibleRsT tShape
	= return []

	-- We can remove type class constraints when we have a matching instance in the table
	--	These can be converted to a direct function call in the CoreIR, so we don't need to
	--	pass dictionaries at runtime.
	--
 	| FConstraint v ts	<- ff
	, Just instances	<- Map.lookup v classInstances
	, Just inst'		<- find (matchInstance ff) instances
	= return []

	-- Purity constraints on bottom effects can be removed.
	--	This doesn't give us any useful information.
	| FConstraint v [TBot kE]	<- ff
	, kE	== kEffect
	, v == primPure 
	= return []

	-- Purity constraints on manifest effects can be discharged. 
	--	These can be reconstructed in the CoreIR by using the Const witnesses that will have
	--	been generated when the Purity constraint was crushed.
	| FConstraint v [t]		<- ff
	, v == primPure
	= case t of
		TSum kE _		
			| kE == kEffect	-> return []
		TEffect{}		-> return []
		_			-> return [ff]
	
	-- These compound fetters can be converted to their crushed forms.
	--	Although Type.Crush.Fetter also crushes fetters in the graph, if a scheme is generalised
	--	which contains a fetter acting on a monomorphic class, and then that class is updated,
	--	we'll get a non-crushed fetter when that scheme is re-extracted from the graph
	| FConstraint v [t]		<- ff
	= do	mFs	<- crushFetter ff
		case mFs of
		 Nothing	-> return [ff]
		 Just fs	-> return fs


	-- have to keep this context
	| otherwise
	= return [ff]


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
				 | kindOfType ta == Just k
			 	 -> True

				_	-> False)

		$ concat $ constrs
	= True

	| otherwise
	= False
