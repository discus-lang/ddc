module LlvmM
	( LlvmState
	, LlvmM
	, addBlock
	, addBlockResult
	, addComment
	, currentReg

	, newUniqueReg
	, newUniqueNamedReg
	, newUniqueLabel

	, initLlvmState

	, startFunction
	, endFunction )
where

import Util

import Llvm
import Llvm.GhcReplace.Unique

import qualified Data.Map		as Map


-- LlvmState contains the register for the current 'of interest' data and
-- a list of blocks of statements. The blocks of statements are pushed onto
-- the head of the list and when all statements for a function are available,
-- the list of blocks is reversed and concatenated to produce a list of
-- statements.
data LlvmState
	= LS
	{ freg		:: Maybe LlvmVar
	, fblocks	:: [[LlvmStatement]]
	, globals	:: Map String LMGlobal }

type LlvmM = StateT LlvmState IO


initLlvmState :: LlvmState
initLlvmState = LS { globals = Map.empty, freg = Nothing, fblocks = [] }

addBlock :: [LlvmStatement] -> LlvmM ()
addBlock code
 = do	state	<- get
	modify $ \s -> s { freg = Nothing, fblocks = code : (fblocks state) }


addBlockResult :: LlvmVar -> [LlvmStatement] -> LlvmM ()
addBlockResult result code
 = do	state	<- get
	modify $ \s -> s { freg = Just result, fblocks = code : (fblocks state) }


addComment :: LMString -> LlvmM ()
addComment text
 = do	state	<- get
	modify $ \s -> s { freg = freg state, fblocks = [Comment (lines text)] : (fblocks state) }


currentReg :: LlvmM LlvmVar
currentReg
 = do	state	<- get
	return $ fromJust $ freg state

startFunction :: LlvmM ()
startFunction
 =	modify $ \s -> s { freg = Nothing, fblocks = [] }

endFunction :: LlvmM [LlvmStatement]
endFunction
 = do	-- At end of function reverse the list of blocks and then
	-- concatenate the blocks to produce a list of statements.
	state		<- get
	let blks	= fblocks state
	case blks of
	  []	->	return	[Return Nothing]
	  x:xs	->	return	$ concat
				$ reverse
				$ case last x of
				    Return _	-> blks
				    _		-> [Return Nothing] : blks

--------------------------------------------------------------------------------
{-
addGlobal :: LlvmFunctionDecl -> LlvmM ()
addGlobal fd
 = do	state		<- get
	let map		= globals state
	case Map.lookup name of
	  Nothing	-> return ()
	  Just curr	-> return ()
-}
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

