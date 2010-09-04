{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Bits and pieces for working with types that don't have a better home.
module DDC.Type.Bits
	( takeValueArityOfType)
where
import DDC.Type.Exp
import DDC.Type.Compounds


-- | Take the arity of a type, ie how many arguments we can apply to it.
--	If the type is not a value type this returns Nothing
takeValueArityOfType :: Type -> Maybe Int
takeValueArityOfType tt
 = case tt of
	TNil		-> Nothing
	TForall	_ _ t	-> takeValueArityOfType t
	TConstrain t _	-> takeValueArityOfType t

	TApp{}		
	 | Just (_, t2, _, _)	<- takeTFun tt
	 , Just a2		<- takeValueArityOfType t2
	 -> Just $ 1 + a2
	
	 | _			<- takeTData tt
	 -> Just 0
	
	TSum{}		-> Nothing
	TCon{}		-> Just 0
	TVar{}		-> Just 0
	TError{}	-> Nothing
