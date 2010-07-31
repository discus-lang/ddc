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
--	, slurpClosureToMap
--	, slurpMapToClosure)
where 
--import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Type
-- import DDC.Var
import DDC.Core.Lint.Env
import qualified Debug.Trace
--import qualified Data.Map	as Map
--import Data.Map			(Map)
--import Data.List

--stage		= "DDC.Core.Lint.Base"

debugExp	= False
debugType	= False
debugKind	= False
indenting	= (2 :: Int)
trace ss x	= Debug.Trace.trace (pprStrPlain ss) x

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
	x : xs	-> f x `seq` checkList f xs


subSingleton v t v'
	| TVar _ (UVar v3)	<- t
	, v == v3	= Nothing

	| TVar _ (UMore v3 _)	<- t
	, v == v3	= Nothing
	
	| v == v'	= Just t
	| otherwise	= Nothing

{-
-- TODO: repacking this again and again costs O(n^2) time in size of closure.
slurpClosureToMap :: Closure -> Map Var Type
slurpClosureToMap clo
 	| isTBot clo	= Map.empty
	
	| Just (v, t)	<- takeTFree clo
	= Map.singleton v t
	
	| TSum k ts	<- clo
	, isClosureKind k
	= foldl' (\m1 m2 
		   -> Map.unionWithKey
			(\v c1 c2 -> packType $ makeTSum k [makeTFree v c1, makeTFree v c2])
			m1 m2)
		Map.empty
		$ map slurpClosureToMap ts
	
	| otherwise
	= panic stage $ "slurpClosureToMap: no match for " % clo
	
-- TODO: It'd be better to keep a 
--	map of in-scope vars -> closures
--      as well a a set of out of closures for top-level / out of scope things.
--
slurpMapToClosure :: Map Var Type -> Closure
slurpMapToClosure mm
	= packType 
	$ makeTSum kClosure
	$ map (uncurry makeTFree)
	$ Map.toList mm
-}
	