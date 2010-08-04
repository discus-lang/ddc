module DDC.Core.Lint.Exp
	( checkedTypeOfExp
	, checkExp')
where
import DDC.Core.Exp
import DDC.Type
import DDC.Type.ClosureStore	(ClosureStore)
import Data.Sequence
import {-# SOURCE #-} DDC.Core.Lint.Env

checkedTypeOfExp :: String -> Exp -> Type	
checkExp' 	 :: Int -> Exp -> Env -> (Exp, Type, Seq Effect, ClosureStore)
