{-# OPTIONS -fno-warn-type-defaults #-}

module Llvm.Runtime.Tags
	( tagThunk
	, tagData
	, tagDataR
	, tagDataM
	, tagSuspIndir
	, tagDataRS )
where

import Data.Bits

import Llvm
import Llvm.Util

-- See tagging stuff in:
--	runtime/Object.h
--	runtime/Alloc.ci
--	runtime/Prim/Boxing.ci


-- Tag values from runtime/Object.h.

tagThunk :: LlvmVar
tagThunk = i32LitVar 0x11

tagData :: LlvmVar
tagData = i32LitVar 0x21

tagDataR :: LlvmVar
tagDataR = i32LitVar 0x31

tagDataM :: LlvmVar
tagDataM = i32LitVar 0x41

tagSuspIndir :: LlvmVar
tagSuspIndir = i32LitVar 0x51

tagDataRS :: Int -> LlvmVar
tagDataRS dataSize = i32LitVar ((shiftL dataSize 4) .|. objModeDataRS)


-- objModeForward, objModeFixed
objModeDataRS :: Int
--objModeForward	= 0x00
--objModeFixed	= 0x01
objModeDataRS	= 0x03

