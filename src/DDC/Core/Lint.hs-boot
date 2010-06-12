module DDC.Core.Lint
	(checkExp)
where
import DDC.Core.Lint.Env
import DDC.Core.Exp
import DDC.Type
	
checkExp :: Exp -> Env -> (Type, Effect, Closure)
