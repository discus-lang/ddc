{-# OPTIONS -Wall -Werror -fwarn-unused-imports #-}
module Llvm.Runtime.Slot
	( readSlot
	, writeSlot )
where

import DDC.Main.Error

import Llvm
import Llvm.Runtime.Data
import Llvm.Util

stage :: String
stage = "LLvm.Runtime.Slot"


-- | Store the given variable on the specified GC slot number.
writeSlot :: LlvmVar -> Int -> IO [LlvmStatement]
writeSlot var 0
 = return $ [ Store var localSlotBase ]

writeSlot var n
 | n > 0
 = do	dst	<- newUniqueNamedReg ("slot" ++ show n) pObj
	return	$ [ Assignment dst (GetElemPtr True localSlotBase [llvmWordLitVar n])
		  , Store var (pVarLift dst) ]

writeSlot _ n = panic stage $ "writeSlot with slot == " ++ show n

-------------------------------------------------------------------------------

-- | Read the given slot number and assigned the variable there to the specified
-- variable.
readSlot :: Int -> LlvmVar -> IO [LlvmStatement]
readSlot 0 dstreg
 = return $ [ Assignment dstreg (Load localSlotBase) ]

readSlot n dstreg
 | n > 0
 = do	r0	<- newUniqueReg pObj
	return	$ [ Assignment r0 (GetElemPtr True localSlotBase [llvmWordLitVar n])
		  , Assignment dstreg (Load (pVarLift r0)) ]

readSlot n _ = panic stage $ "readSlot with slot == " ++ show n

