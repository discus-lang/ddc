{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Debugging config, and simple utils.
module DDC.Core.Check.Base
	( debugExp
	, debugType
	, debugKind
	, indenting
	, trace
	, lintList)
where 
import DDC.Main.Pretty
import DDC.Core.Check.Env
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
