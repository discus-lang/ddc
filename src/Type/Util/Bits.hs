
-- | Bits and pieces for working with types.
module Type.Util.Bits
	-- simple
	( takeValueArityOfType
		
	-- crushing
	, crushT

	-- closure
	, dropTFreesIn
		
	-- contexts
	, addContext
	, addContexts
	
	-- supertypes
	, unflattenSuper
	
	-- stripping
	, stripToBodyT
	)
where
import Util
import DDC.Type.Exp
import DDC.Type.Builtin
import DDC.Type.Compounds
import DDC.Type.Transform
import DDC.Var
import qualified Data.Set	as Set


-- Arity ------------------------------------------------------------------------------------------
-- | Take the arity of a type, ie how many arguments we can apply to it.
--	If the type is not a value type this returns Nothing
takeValueArityOfType :: Type -> Maybe Int
takeValueArityOfType tt
 = case tt of
	TNil		-> Nothing
	TForall	b k t	-> takeValueArityOfType t
	TFetters t fs	-> takeValueArityOfType t
	TConstrain t cs	-> takeValueArityOfType t

	TApp{}		
	 | Just (t1, t2, eff, clo)	<- takeTFun tt
	 , Just a2			<- takeValueArityOfType t2
	 -> Just $ 1 + a2
	
	 | Just _			<- takeTData tt
	 -> Just 0
	
	TSum{}		-> Nothing
	TCon{}		-> Just 0
	TVar{}		-> Just 0
	TError{}	-> Nothing
	

-- Crushing ----------------------------------------------------------------------------------------
-- | do some simple packing
crushT :: Type -> Type
crushT t
 = let	t'	= transformT crushT1 t
   in	if t == t'
   	 then	t
	 else 	crushT t'

crushT1 :: Type -> Type
crushT1 tt
 = case tt of
 	TSum k ts			
	 -> makeTSum k 		$ flattenTSum tt

	TApp{}
	 | Just (v,  t2)	<- takeTFree tt
	 , Just (v', t)		<- takeTFree t2
	 -> makeTFree v t
	
	 | Just (v, t2)		<- takeTFree tt
	 , TSum k ts		<- t2
	 -> makeTSum k $ map (makeTFree v) ts
	
	_	-> tt

-- | Drop TFree terms concerning value variables in this set
dropTFreesIn :: Set Var -> Closure -> Closure
dropTFreesIn vs clo
 	= makeTSum kClosure
	$ filter (\c -> case takeTFree c of
			 Just (v, _)	-> not $ Set.member v vs
			 Nothing	-> True)
	$ flattenTSum clo

	
-- | Add a new type class context to a type,
--	pushing it under any enclosing foralls.
addContext :: Kind -> Type -> Type
addContext k tt
 = case tt of
 	TForall b@BVar{}  k t	-> TForall b k (addContext k t)
 	TForall b@BMore{} k t	-> TForall b k (addContext k t)
	_			-> TForall BNil k tt

addContexts :: [Kind] -> Type -> Type
addContexts []	   t	= t
addContexts (k:ks) t	
	= TForall BNil k (addContexts ks t)


-- | Create a superkind.
unflattenSuper :: [Kind] -> Super -> Super
unflattenSuper [] 	s	= s
unflattenSuper (k:ks) 	s	= SFun k (unflattenSuper ks s)
	

-- | Strip off TForalls, TFetters and TContext of a type to get the body of the type.
stripToBodyT :: Type -> Type
stripToBodyT tt
 = case tt of
 	TForall  v k t		-> stripToBodyT t
	TFetters t fs		-> stripToBodyT t
	_			-> tt
