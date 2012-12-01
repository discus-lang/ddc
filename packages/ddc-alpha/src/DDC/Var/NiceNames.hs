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
allocNiceName 
	:: (String -> Bool)	-- ^ Only use names that match this predicate.
	-> NameSpace		-- ^ Use a name appropriate to this namespace.
	-> NiceNames		-- ^ The nice names currently available.
	-> (NiceNames, String)
	
allocNiceName pCanUse space mpNames
 = let	Just names	= Map.lookup space mpNames
	name		= head names
	names'		= tail names
	mpNames'	= Map.insert space names' mpNames

	-- If this name is not acceptable to the predicate then try again.
   in	if pCanUse name 
	 then (mpNames', name)
	 else allocNiceName pCanUse space mpNames'


-- | Make a substitution that maps vars to ones with nicer names.
makeNiceVarSub 
	:: (String -> Bool)	-- ^ Only use names that match this predicate.
	-> NiceNames		-- ^ The nice names currently available.
	-> [Var]		-- ^ Vars to allocate new names for.
	-> (NiceNames, Map Var Var)

makeNiceVarSub pCanUse names vars
 = let	(names', vvSub)	= mapAccumL (makeNiceVarSub1 pCanUse) names vars
   in	(names', Map.fromList $ catMaybes vvSub)


makeNiceVarSub1 
	:: (String -> Bool)
	-> NiceNames
	-> Var
	-> (NiceNames, Maybe (Var, Var))

makeNiceVarSub1 pCanUse names var
 = let	(names', name)	= allocNiceName pCanUse (varNameSpace var) names
	var'		= var { varName = name }
   in	(names', Just (var, var'))
