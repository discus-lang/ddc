module DDC.Core.Lint.Exp
	(checkExp')
where
import DDC.Core.Lint.Env
import DDC.Core.Exp
import DDC.Type
import DDC.Type.ClosureStore	(ClosureStore)
import Data.Sequence

	
checkExp' :: Int -> Exp -> Env -> (Type, Seq Effect, ClosureStore)
