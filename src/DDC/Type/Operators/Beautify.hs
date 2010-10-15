{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Beautification of the names in a type.
module DDC.Type.Operators.Beautify
	( beautifyNamesT
	, beautifyLocalNamesT)
where
import DDC.Type.Operators.Substitute
import DDC.Type.Collect
import DDC.Type.Compounds
import DDC.Type.Exp
import DDC.Var
import qualified Data.Foldable	as Foldable
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import Util

-- | Beautify all the names in a type, including names of unquantified vars.
beautifyNamesT :: Type -> Type
beautifyNamesT tt
 = let	bs		= texBound tt
	vs		= nub $ mapMaybe takeVarOfBound $ Foldable.toList bs
	(_, sub)	= makeNiceVarSub allNiceNames vs
   in	subVV_everywhere sub tt


-- | Beautify the names of locally bound variables.
beautifyLocalNamesT :: Type -> Type
beautifyLocalNamesT tt
 = let	vs		= Set.toList $ collectBindingVarsT tt
	(_, sub)	= makeNiceVarSub allNiceNames vs
   in	subVV_everywhere sub tt


-- Nice names -----------------------------------------------------------------
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
