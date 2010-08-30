{-# OPTIONS -Wall -Werror -fwarn-unused-imports #-}
module Llvm.Runtime.Slot
	( readSlot
	, readSlotVar
	, writeSlot )
where

import DDC.Main.Error

import Llvm
import LlvmM
import Llvm.Runtime.Data
import Llvm.Util

stage :: String
stage = "LLvm.Runtime.Slot"


-- | Store the given variable on the specified GC slot number.
writeSlot :: LlvmVar -> Int -> LlvmM ()
writeSlot var 0
 = addBlock	[ Comment [ "slot [0] = " ++ show var ]
		, Store var localSlotBase ]

writeSlot var n
 | n > 0
 = do	dst	<- newUniqueNamedReg ("slot" ++ show n) pObj
	addBlock
		[ Comment [ "slot [" ++ show n ++ "] = " ++ show var ]
		, Assignment dst (GetElemPtr True localSlotBase [llvmWordLitVar n])
		, Store var (pVarLift dst) ]

writeSlot _ n = panic stage $ "writeSlot with slot == " ++ show n

-------------------------------------------------------------------------------

-- | Read the given slot number and assigned the variable there to the specified
-- variable.
readSlot :: Int -> LlvmM LlvmVar
readSlot 0
 = do	dstreg		<- newUniqueNamedReg "slot.0" pObj
	addBlock	[ Comment [ show dstreg ++ " = readSlot 0" ]
			, Assignment dstreg (Load localSlotBase) ]
	return		dstreg

readSlot n
 | n > 0
 = do	dstreg		<- newUniqueNamedReg ("slot." ++ show n) pObj
	r0		<- newUniqueReg pObj
	addBlock	[ Comment [ show dstreg ++ " = readSlot " ++ show n ]
			, Assignment r0 (GetElemPtr True localSlotBase [llvmWordLitVar n])
			, Assignment dstreg (loadAddress r0) ]
	return		dstreg

readSlot n = panic stage $ "readSlot with slot == " ++ show n


readSlotVar :: Int -> LlvmVar -> LlvmM ()
readSlotVar 0 dstreg
 = 	addBlockResult	dstreg
		[ Comment [ show dstreg ++ " = readSlotVar 0" ]
		, Assignment dstreg (Load localSlotBase) ]

readSlotVar n dstreg
 | n > 0
 = do	r0		<- newUniqueReg pObj
	addBlockResult	dstreg
		[ Comment [ show dstreg ++ " = readSlotVar " ++ show n ]
		, Assignment r0 (GetElemPtr True localSlotBase [llvmWordLitVar n])
		, Assignment dstreg (loadAddress r0) ]

readSlotVar n _ = panic stage $ "readSlotVar with slot == " ++ show n

