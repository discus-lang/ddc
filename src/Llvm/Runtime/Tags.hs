{-# OPTIONS -fno-warn-type-defaults #-}

module Llvm.Runtime.Tags
	( tagBasePlus
	, tagData
	, tagDataM
	, tagDataR
	, tagSuspIndir
	, tagDataRS
	, tagIndir
	, tagSusp
	, tagFixedThunk
	)
where

import Llvm
import Llvm.Util

-- See tagging stuff in:
--	runtime/Object.h
--	runtime/Alloc.ci
--	runtime/Prim/Boxing.ci


tagBasePlus :: Int -> LlvmVar
tagBasePlus x = i32LitVar x

-- These tag values must be compatible with those in runtime/Object.h.

tagData :: Int -> LlvmVar
tagData tag = i32LitVar (objFixedData + tag * 256)

tagDataM :: Int -> LlvmVar
tagDataM tag = i32LitVar (objFixedDataM + tag * 256)

tagDataR :: Int -> LlvmVar
tagDataR tag = i32LitVar (objFixedDataR + tag * 256)

tagDataRS :: Int -> Int -> LlvmVar
tagDataRS tag dataSize = i32LitVar (objModeDataRS + dataSize * 16 + tag * 256)

tagSuspIndir :: LlvmVar
tagSuspIndir = i32LitVar 0x51

tagIndir :: LlvmVar
tagIndir = i32LitVar 0x0fffffd

tagSusp :: LlvmVar
tagSusp = i32LitVar 0x0fffffe

tagFixedThunk :: LlvmVar
tagFixedThunk = i32LitVar  ((0x0ffffff * 256) + 0x11)

-- objModeForward, objModeFixed
objModeDataRS :: Int
--objModeForward	= 0x00
--objModeFixed	= 0x01
objModeDataRS	= 0x03

objFixedData :: Int
objFixedData	= 0x21

objFixedDataR :: Int
objFixedDataR = 0x31

objFixedDataM :: Int
objFixedDataM = 0x41


