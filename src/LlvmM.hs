module LlvmM
	( LlvmState
	, LlvmM
	, addBlock
	, addComment

	, addAlias

	, addGlobalVar
	, addGlobalFuncDecl
	, addString

	, newUniqueReg
	, newUniqueNamedReg
	, newUniqueLabel
	, newUniqueName
	, newNamedReg

	, initLlvmState

	, startFunction
	, endFunction

	, setTags
	, getTag

	, getModuleId

	, renderModule )
where

import DDC.Main.Error
import DDC.Var.ModuleId

import Util

import Llvm
import Llvm.GhcReplace.Unique
import Llvm.Runtime.Object
import Llvm.Util

import Data.Char

import qualified Data.Map		as Map


stage = "LlvmM"


data LlvmState
	= LS
	-- Temporary variable to hold the current  list of blocks of statements.
	-- The blocks of statements are pushed onto the head of the list and
	-- when all statements for a function are available, the list of blocks
	-- is reversed and concatenated to produce a list of statements, which
	-- can then be pushed onto the functions list below.
	{ tmpBlocks	:: [[LlvmStatement]]

	-- | The module Id of the current module.
	, moduleId	:: ModuleId

	-- | Aliases used in the module.
	, aliases	:: Map String LlvmType

	-- | Global variables for the module.
	, globalVars	:: Map String LMGlobal

	-- | Global variables for the module.
	, strings	:: Map String LMGlobal

	-- | Forward declarations of external functions.
	, funcDecls	:: Map String LlvmFunctionDecl

	-- | Functions defined in this module.
	, functions	:: [LlvmFunction]

	-- | The constructor tags for the module.
	, ctorTags	:: Map String Int }

type LlvmM = StateT LlvmState IO


initLlvmState :: ModuleId -> LlvmState
initLlvmState modId
 = LS	{ tmpBlocks	= []

	, moduleId	= modId
	, aliases	= Map.empty
	, globalVars	= Map.empty
	, strings	= Map.empty
	, funcDecls	= Map.empty
	, functions	= []
	, ctorTags	= Map.empty }


addBlock :: [LlvmStatement] -> LlvmM ()
addBlock code
 = do	state	<- get
	modify $ \s -> s
		{ tmpBlocks = code : tmpBlocks state }


addComment :: LMString -> LlvmM ()
addComment text
 = do	state	<- get
	modify $ \s -> s
		{ tmpBlocks = [Comment (lines text)] : tmpBlocks state }


addGlobalVar :: LMGlobal -> LlvmM ()
addGlobalVar (gvar, init)
 = do	state		<- get
	let map		= globalVars state
	let name	= getPlainName gvar
	case Map.lookup name map of
	  Nothing	-> modify $ \s -> s { globalVars = Map.insert name (gvar, init) map }
	  Just (cur, _)	-> unless (getVarType cur == getVarType gvar)
				$ panic stage
					$ "addGlobalVar: The following two should match :"
					++ "\n    '" ++ show cur
					++ "'\n    '" ++ show gvar ++ "'"

addString :: String -> LlvmM LlvmVar
addString s
 = do	vname	<- newUniqueName "str"
	let gvar = LMGlobalVar vname (typeOfString s) Internal Nothing ptrAlign True
	let init = Just (LMStaticStr (escapeString s) (typeOfString s))
	state		<- get
	let map		= strings state
	case Map.lookup s map of
	  Nothing
	    ->	do	modify (\st -> st { strings = Map.insert s (gvar, init) map })
			return gvar

	  Just (cur, _)
	    -> do	unless (getVarType cur == getVarType gvar)
			 $ panic stage
				$ "addString: The following two should match :"
				++ "\n    '" ++ show cur
				++ "'\n    '" ++ show gvar ++ "'"
			return cur


startFunction :: LlvmM ()
startFunction
 =	modify $ \s -> s { tmpBlocks = [] }


endFunction :: LlvmFunctionDecl -> [LMString] -> [LlvmFuncAttr] -> LMSection -> LlvmM ()
endFunction funcDecl funcArgs funcAttrs funcSect
 = do	-- At end of function reverse the list of blocks and then
	-- concatenate the blocks to produce a list of statements.
	state		<- get
	let fblks	= tmpBlocks state
	let blks	=
			if null fblks
			  then [Return Nothing]
			  else concat $ reverse $
				case last (head fblks) of
				  Return _	-> fblks
				  _		-> [Return Nothing] : fblks

	let func	= LlvmFunction funcDecl funcArgs funcAttrs funcSect
				$ blockify (LlvmBlock (fakeUnique "entry") blks)

	modify $ \s -> s { functions = func : functions s }


blockify :: LlvmBlock -> [LlvmBlock]
blockify (LlvmBlock id stmts)
 = case chopWhenLeft isLabel stmts of
	head : tail -> LlvmBlock id head : map convBlock tail
	_ -> error "Ooops!"

isLabel :: LlvmStatement -> Bool
isLabel (MkLabel _) = True
isLabel _ = False

convBlock :: [LlvmStatement] -> LlvmBlock
convBlock (MkLabel id:tail) = LlvmBlock id tail
convBlock _ = error "convBlock"

--------------------------------------------------------------------------------

addAlias :: LlvmAlias -> LlvmM ()
addAlias (name, typ)
 = do	state		<- get
	let map		= aliases state
	case Map.lookup name map of
	  Nothing	-> modify $ \s -> s { aliases = Map.insert name typ map }
	  Just curr	-> unless (curr == typ)
				$ panic stage
					$ "addAlias: The following two should match :"
					++ "\n    " ++ show curr
					++ "\n    " ++ show typ


addGlobalFuncDecl :: LlvmFunctionDecl -> LlvmM ()
addGlobalFuncDecl fd
 = do	checkFnName	$ nameOfFunDecl fd
	state		<- get
	let map		= funcDecls state
	let name	= nameOfFunDecl fd
	case Map.lookup name map of
	  Nothing	-> modify $ \s -> s { funcDecls = Map.insert name fd map }
	  Just curr	-> unless (curr == fd)
				$ panic stage
					$ "addGlobalFuncDecl: The following two should match :"
					++ "\n    " ++ show curr
					++ "\n    " ++ show fd


checkFnName :: String -> LlvmM ()
checkFnName fname
 = do	when (null fname || not (checkName True fname))
		$ panic stage $ "\nBad function name '" ++ fname ++ "'\n"
	return ()

checkName :: Bool -> String -> Bool
checkName ok [] = ok
checkName ok (s:ss)
 | isAlphaNum s || s == '_'
 = checkName ok ss

 | otherwise
 = False

--------------------------------------------------------------------------------

setTags :: [(String, Int)] -> LlvmM ()
setTags lst
 = do	state		<- get
	let ctors	= ctorTags state
	modify		$ \s -> s { ctorTags = foldl' insertTag ctors lst }

insertTag :: Map String Int -> (String, Int) -> Map String Int
insertTag map (name, value)
 = case Map.lookup name map of
	Nothing	-> Map.insert name value map
	Just v	-> if value == v
			then map
			else panic stage
				$ "Ctor name mismatch for '" ++ name ++ "' :"
					++ "\n    " ++ show value
					++ "\n    " ++ show v

getTag :: String -> LlvmM Int
getTag name
 = do	state		<- get
	let ctors	= ctorTags state
	case Map.lookup name ctors of
	  Just v	-> return v
	  Nothing	-> panic stage $ "Can't find Ctor tag '" ++ name ++ "'."

getModuleId :: LlvmM ModuleId
getModuleId
 = do	state	<- get
	return	$ moduleId state

--------------------------------------------------------------------------------

renderModule :: [LMString] ->  LlvmM LlvmModule
renderModule comments
 = do	state		<- get
	let taliases	= Map.toList $ aliases state
	let globals	= map snd $ Map.toList $ globalVars state
	let strs	= map snd $ Map.toList $ strings state
	let fdecls	= map snd $ Map.toList $ funcDecls state
	return	$ LlvmModule comments taliases (globals ++ strs) fdecls
				$ reverse $ functions state

--------------------------------------------------------------------------------

-- | Generate a new unique register variable with the specified LlvmType.
newUniqueReg :: LlvmType -> LlvmM LlvmVar
newUniqueReg t
 = do	u <- lift $ newUnique "r"
	return $ LMLocalVar u t


-- | Generate a new unique named register variable with the specified LlvmType.
newUniqueNamedReg :: String -> LlvmType -> LlvmM LlvmVar
newUniqueNamedReg name t
 = do	u <- lift $ newUnique name
	return $ LMLocalVar u t


-- | Generate a new unique register variable.
newUniqueLabel :: String -> LlvmM LlvmVar
newUniqueLabel label
 = do	u <- lift $ newUnique label
	return $ LMLocalVar u LMLabel


newUniqueName :: String -> LlvmM String
newUniqueName name
 = do	u <- lift $ newUnique name
	return $ show u

newNamedReg :: String -> LlvmType -> LlvmM LlvmVar
newNamedReg name t
 =	return $ LMLocalVar (fakeUnique name) t


