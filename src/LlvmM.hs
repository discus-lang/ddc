module LlvmM
	( LlvmState
	, LlvmM
	, addBlock
	, addBlockResult
	, addComment
	, currentReg

	, initLlvmState

	, startFunction
	, endFunction )
where

import Util

import Llvm


-- LlvmState contains the register for the current 'of interest' data and
-- a list of blocks of statements. The blocks of statements are pushed onto
-- the head of the list and when all statements for a function are available,
-- the list of blocks is reversed and concatenated to produce a list of
-- statements.
data LlvmState
	= LS
	{ freg :: Maybe LlvmVar
	, fblocks :: [[LlvmStatement]] }

type LlvmM = StateT LlvmState IO


initLlvmState :: LlvmState
initLlvmState = LS { freg = Nothing, fblocks = [] }

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
