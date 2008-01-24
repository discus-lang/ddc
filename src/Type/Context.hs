
-- | Functions for reducing the context of a type.

module Type.Context
	( reduceContextT )

where

------
import Type.Exp
import Type.Plate
import Type.Util
import Shared.VarPrim
import Shared.Error

import Util

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Data.Map	as Map
import Data.Map			(Map)

import Debug.Trace
-----

stage	= "Type.Context"


-- | Reduce the context of this type using the provided map of instance definitions.

reduceContextT 
	:: Map Var [Fetter]	-- (class var -> instances) for this class
	-> Type			-- the type to reduce  
	-> Type
	
reduceContextT classInst tt
 = case tt of
 	TFetters fs t	
	 -> let	vsFreeT	= freeVars t
	 	fs'	= concat
	 		$ map (reduceContextF vsFreeT classInst) fs

	    in	case fs' of
	    		[]	-> t
			_	-> TFetters fs' t
			
	_ 		-> tt
	

reduceContextF 
	:: Set Var		-- vars that are free in the shape of the type
	-> Map Var [Fetter]	-- (class var -> instances) for each class
	-> Fetter		-- the constraint being used
	-> [Fetter]		-- maybe some new constraints

reduceContextF vsShape classInstances ff
--  = trace (pprStr $ "reduceContextF: " % ff % "\n")
 = reduceContextF' vsShape classInstances ff

reduceContextF' vsShape classInstances ff

	-- If there is a Const, Lazy, Mutable, Direct fetter on a region that isn't in the shape of the
	--	type then we can safely ditch it
	| FConstraint v [TVar KRegion r]	<- ff
	, v == primConst -- elem v [primConst, primMutable, primMutable, primDirect]
	, not $ Set.member r vsShape
	= []


	-- If there is a matching instance for this class then we can remove the constraint.
 	| FConstraint v ts	<- ff
	, Just instances	<- Map.lookup v classInstances
	, Just inst'		<- find (matchInstance ff) instances
	= []

	-- Purity constraints on bottom effects can be removed.
	| FConstraint v [TBot KEffect]	<- ff
	, v == primPure 
	= []

	-- Purity constraints on read effects can be dischared by 
	--	making the region constant
	| FConstraint v effs	<- ff
	, v == primPure
	, effsFlat		<- catMap flattenTSum effs
	, (effs', fsMore)	<- unzip $ map purifyEff effsFlat

	-- build the effs we couldn't purify back into a sum
	, remainingEff		<- makeTSum KEffect $ catMaybes effs'

	-- this produces constraints (like Const) that might be able to be further reduced,
	--	so make sure to call ourselves recursively..
	= case remainingEff of
	    TBot KEffect	
	      -> catMap (reduceContextF vsShape classInstances) 
	      $ catMaybes fsMore

	    _ -> FConstraint primPure [remainingEff] 
	      :  (   catMap (reduceContextF vsShape classInstances)
	          $  catMaybes fsMore)
	    

	
	| otherwise
	= [ff]


purifyEff eff
 	| TEffect v [tR@(TVar KRegion r)]	<- eff
	, v == primRead
	= (Nothing, Just (FConstraint primConst [tR]))

 	| TEffect v [tR@(TClass KRegion _)]	<- eff
	, v == primRead
	= (Nothing, Just (FConstraint primConst [tR]))
	
	| otherwise
	= (Just eff, Nothing)



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
