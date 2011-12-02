module DDC.Core.Check.Exp
	( checkedTypeOfExp
	, checkedTypeOfOpenExp
	, checkExp')
where
import DDC.Core.Exp
import DDC.Type
import DDC.Type.EffectStore	(EffectStore)
import DDC.Type.ClosureStore	(ClosureStore)
import {-# SOURCE #-} DDC.Core.Check.Env

checkedTypeOfExp 	:: String -> Exp -> Type	
checkedTypeOfOpenExp	:: String -> Exp -> Type
checkExp' 	 	:: Int -> Exp -> Env -> (Exp, Type, EffectStore, ClosureStore)
