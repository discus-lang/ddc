module DDC.Core.Lint.Exp
	(checkExp')
where
import DDC.Core.Lint.Env
import DDC.Core.Exp
import DDC.Type
import DDC.Var
import Data.Sequence
import Data.Map
	
checkExp' :: Int -> Exp -> Env -> (Type, Seq Effect, Map Var Closure)
