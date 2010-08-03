{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Core.Lint.Base
	( debugExp
	, debugType
	, debugKind
	, indenting
	, trace
	, lintList
	, checkList
	, subSingleton)
where 
import DDC.Main.Pretty
import DDC.Type
import DDC.Core.Lint.Env
import qualified Debug.Trace

debugExp	= False
debugType	= False
debugKind	= False
indenting	= (2 :: Int)
trace ss x	= Debug.Trace.trace (pprStrPlain ss) x

-- | Check for lint in some list of things.
lintList ::  (a -> Env -> a) -> [a] -> Env -> [a]
lintList lintFun xx env
 = case xx of
	[]		-> []
	(x:xs)		
	 | x'	<- lintFun  x env
	 , xs'	<- lintList lintFun xs env
	 -> x `seq` xs `seq` x' : xs'

checkList :: (a -> ()) -> [a] -> ()
checkList f xx
 = case xx of
	[]	-> ()
	x : xs	-> f x `seq` checkList f xs


subSingleton v t v'
	| TVar _ (UVar v3)	<- t
	, v == v3	= Nothing

	| TVar _ (UMore v3 _)	<- t
	, v == v3	= Nothing
	
	| v == v'	= Just t
	| otherwise	= Nothing
	