
module DDC.Core.Lint.Base
	( lintList
	, checkList
	, subSingleton)
where 
import DDC.Type
import DDC.Core.Lint.Env


-- | Check for lint in some list of things.
lintList ::  (a -> Env -> b) -> [a] -> Env -> ()
lintList lintFun xx env
 = case xx of
	[]		-> ()
	(x:xs)		
		->    lintFun  x env
		`seq` lintList lintFun xs env
		`seq` ()

checkList :: (a -> ()) -> [a] -> ()
checkList f xx
 = case xx of
	[]	-> ()
	x : xs	-> f x `seq` checkList f xs `seq` ()


subSingleton v t v'
	| TVar _ (UVar v3)	<- t
	, v == v3	= Nothing

	| TVar _ (UMore v3 _)	<- t
	, v == v3	= Nothing
	
	| v == v'	= Just t
	| otherwise	= Nothing
