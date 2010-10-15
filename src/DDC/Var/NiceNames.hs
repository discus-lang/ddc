{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Var.NiceNames
	( NiceNames
	, allNiceNames
	, allocNiceName
	, makeNiceVarSub)
where
import DDC.Var
import qualified Data.Map	as Map
import Util

-- | Nice names are the ones that we prefer to use in types displayed to the user.
--   When the compiler generates internal names they're named after the compiler
--   stage that generated them, like eDE1123. The number is used to uniquify 
--   the variable across the entire module. However, error messages etc are easier
--   to read if we use good old e0, e1 etc.
--
type NiceNames	
	= Map NameSpace [String]

allNiceNames :: Map NameSpace [String]
allNiceNames
 	= Map.insert NameType    ["t" ++ show i | i <- [0 :: Int ..]]
	$ Map.insert NameRegion  ["r" ++ show i | i <- [0 :: Int ..]]
	$ Map.insert NameEffect  ["e" ++ show i | i <- [0 :: Int ..]]
	$ Map.insert NameClosure ["c" ++ show i | i <- [0 :: Int ..]] 
	$ Map.empty


-- | Allocate a new nice names of a given namespace.
allocNiceName :: NameSpace -> NiceNames -> (NiceNames, String)
allocNiceName space mpNames
 = let	Just names	= Map.lookup space mpNames
	name		= head names
	names'		= tail names
   in	(Map.insert space names' mpNames, name)


-- | Make a substitution that maps vars to ones with nicer names.
makeNiceVarSub :: NiceNames -> [Var] -> (NiceNames, Map Var Var)
makeNiceVarSub names vars
 = let	(names', vvSub)	= mapAccumL makeNiceVarSub1 names vars
   in	(names', Map.fromList $ catMaybes vvSub)

makeNiceVarSub1 :: NiceNames -> Var -> (NiceNames, Maybe (Var, Var))
makeNiceVarSub1 names var
 = let	(names', name)	= allocNiceName (varNameSpace var) names
	var'		= var { varName = name }
   in	(names', Just (var, var'))
