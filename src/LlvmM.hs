module LlvmM
	( LlvmState
	, LlvmM
	, addBlock
	, addBlockResult
	, addComment
	, currentReg )
where

import Util

import Llvm


-- LlvmState contains the register for the current 'of interest' data and
-- a list of blocks of statements. The blocks of statements are pushed onto
-- the head of the list and when all statements for a function are available,
-- the list of blocks is reversed and concatenated to produce a list of
-- statements.
type LlvmState = (Maybe LlvmVar, [[LlvmStatement]])

type LlvmM = StateT LlvmState IO


addBlock :: [LlvmStatement] -> LlvmM ()
addBlock code
 = do	state	<- get
	put (Nothing, code : (snd state))


addBlockResult :: LlvmVar -> [LlvmStatement] -> LlvmM ()
addBlockResult result code
 = do	state	<- get
	put (Just result, code : (snd state))


addComment :: LMString -> LlvmM ()
addComment text
 = do	(reg, code)	<- get
	put (reg, [Comment [text]] : code)


currentReg :: LlvmM LlvmVar
currentReg
 = do	(Just reg, _)	<- get
	return reg


