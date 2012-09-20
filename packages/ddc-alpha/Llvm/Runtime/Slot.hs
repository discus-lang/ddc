{-# OPTIONS -Wall -Werror -fwarn-unused-imports #-}
module Llvm.Runtime.Slot
	( readSlot
	, writeSlot )
where

import DDC.Main.Error

import Llvm
import LlvmM
import Llvm.Runtime.Object
import Llvm.Runtime.Data
import Llvm.Util

stage :: String
stage = "LLvm.Runtime.Slot"


-- | Store the given variable on the specified GC slot number.
writeSlot :: LlvmVar -> Int -> LlvmM ()
writeSlot var 0
 = addBlock	[ Store var localSlotBase ]

writeSlot var n
 | n > 0
 = do	dst		<- newUniqueNamedReg ("slot" ++ show n) pObj
	addBlock	[ Assignment dst (GetElemPtr True localSlotBase [llvmWordLitVar n])
			, Store var (pVarLift dst) ]

writeSlot _ n = panic stage $ "writeSlot with slot == " ++ show n

-------------------------------------------------------------------------------

-- | Read the given slot number and assigned the variable there to the specified
-- variable.
readSlot :: Int -> LlvmM LlvmVar
readSlot 0
 = do	dstreg		<- newUniqueNamedReg "slot.0" pObj
	addBlock	[ Assignment dstreg (Load localSlotBase) ]
	return		dstreg

readSlot n
 | n > 0
 = do	dstreg		<- newUniqueNamedReg ("slot." ++ show n) pObj
	r0		<- newUniqueReg pObj
	addBlock	[ Assignment r0 (GetElemPtr True localSlotBase [llvmWordLitVar n])
			, Assignment dstreg (loadAddress r0) ]
	return		dstreg

readSlot n = panic stage $ "readSlot with slot == " ++ show n

