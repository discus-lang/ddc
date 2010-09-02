module LlvmM
	( LlvmState
	, LlvmM
	, addBlock
	, addBlockResult
	, addComment
	, currentReg

	, addGlobalFuncDecl

	, newUniqueReg
	, newUniqueNamedReg
	, newUniqueLabel

	, initLlvmState

	, startFunction
	, endFunction

	, renderModule )
where

import DDC.Main.Error

import Util

import Llvm
import Llvm.GhcReplace.Unique
import Llvm.Util

import qualified Data.Map		as Map


stage = "LlvmM"


-- LlvmState contains the register for the current 'of interest' data and
-- a list of blocks of statements. The blocks of statements are pushed onto
-- the head of the list and when all statements for a function are available,
-- the list of blocks is reversed and concatenated to produce a list of
-- statements.
data LlvmState
	= LS
	{ freg		:: Maybe LlvmVar
	, fblocks	:: [[LlvmStatement]]

	, funcDecls	:: Map String LlvmFunctionDecl
	, functions	:: [LlvmFunction] }

type LlvmM = StateT LlvmState IO


initLlvmState :: LlvmState
initLlvmState
 = LS	{ freg = Nothing
	, fblocks = []
	, funcDecls = Map.empty
	, functions = [] }

addBlock :: [LlvmStatement] -> LlvmM ()
addBlock code
 = do	state	<- get
	modify $ \s -> s
		{ freg = Nothing
		, fblocks = code : (fblocks state) }


addBlockResult :: LlvmVar -> [LlvmStatement] -> LlvmM ()
addBlockResult result code
 = do	state	<- get
	modify $ \s -> s
		{ freg = Just result
		, fblocks = code : (fblocks state) }


addComment :: LMString -> LlvmM ()
addComment text
 = do	state	<- get
	modify $ \s -> s
		{ freg = freg state
		, fblocks = [Comment (lines text)] : (fblocks state) }


currentReg :: LlvmM LlvmVar
currentReg
 = do	state	<- get
	return $ fromJust $ freg state

startFunction :: LlvmM ()
startFunction
 =	modify $ \s -> s { freg = Nothing, fblocks = [] }

endFunction :: LlvmFunctionDecl -> [LMString] -> [LlvmFuncAttr] -> LMSection -> LlvmM ()
endFunction funcDecl funcArgs funcAttrs funcSect
 = do	-- At end of function reverse the list of blocks and then
	-- concatenate the blocks to produce a list of statements.
	state		<- get
	let fblks	= fblocks state
	let blks	=
			if null fblks
			  then [Return Nothing]
			  else concat $ reverse $
				case last (head fblks) of
				  Return _	-> fblks
				  _		-> [Return Nothing] : fblks

	let func	= LlvmFunction funcDecl funcArgs funcAttrs funcSect
				[ LlvmBlock (fakeUnique "entry") blks ]

	modify $ \s -> s { functions = func : (functions s) }

--------------------------------------------------------------------------------

addGlobalFuncDecl :: LlvmFunctionDecl -> LlvmM ()
addGlobalFuncDecl fd
 = do	state		<- get
	let map		= funcDecls state
	let name	= nameOfFunDecl fd
	case Map.lookup name map of
	  Nothing	-> modify $ \s -> s { funcDecls = Map.insert name fd map }
	  Just curr	-> if curr == fd
				then return ()
				else panic stage
					$ "The following two should match :"
					++ "\n    " ++ show curr
					++ "\n    " ++ show fd

--------------------------------------------------------------------------------

renderModule :: [LMString] -> [LlvmAlias] -> [LMGlobal] -> LlvmM LlvmModule
renderModule comments aliases globals
 = do	state		<- get
	let fdecls	= map snd $ Map.toList $ funcDecls state
	return	$ LlvmModule comments aliases globals fdecls
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

