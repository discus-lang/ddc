{-# OPTIONS -fwarn-unused-imports -fno-warn-type-defaults -cpp #-}

-- | Define DDC structs.
-- These definitions need to be compatible across code compile compiled via the
-- C backend and the LLVM backend. They need to be kept in sync with the ones
-- in runtime/Object.h.

module Llvm.Runtime.Object
	( genericFunPtrType
	, ptrAlign

	, ddcObj
	, ddcThunk
	, ddcData
	, ddcDataRS

	, structObj
	, structThunk
	, structData
	, structDataRS

	, nullObj

	, pObj
	, ppObj
	, pStructThunk
	, pStructData
	, pStructDataRS

	, objModeForward
	, objModeFixed
	, objModeDataRS )
where

import Llvm
import Llvm.Runtime.Struct

import qualified Config.Config		as Config


-- Types and variables.

ddcObj :: LlvmType
ddcObj = LMStruct [ i32 ]

structObj :: LlvmType
structObj = LMAlias ("struct.Obj", ddcObj)

pObj :: LlvmType
pObj = pLift structObj

ppObj :: LlvmType
ppObj = pLift pObj

nullObj :: LlvmVar
nullObj = LMLitVar (LMNullLit pObj)


-- | A generic function pointer which takes no parameters and returns void.
-- Basically :    typedef void (*FunPtr) (void)
genericFunPtrType :: LlvmType
genericFunPtrType
 = LMPointer (LMFunction (
	LlvmFunctionDecl "generic.function.ptr.type" Internal CC_Ccc LMVoid FixedArgs [] ptrAlign ))


ptrAlign :: Maybe Int
ptrAlign = Just Config.pointerBytes

--------------------------------------------------------------------------------

ddcThunk :: LlvmStructDesc
ddcThunk
 =	mkLlvmStructDesc "Thunk"
		[ AField "tag" i32			-- tag
		, APadTo8If64
		, AField "funptr" genericFunPtrType	-- function pointer
		, AField "arity" i32			-- arity
		, AField "argc" i32			-- arg count
		, APadTo8If64
		, AField "args" (LMArray 0 pObj) ]	-- array of arguments


structThunk :: LlvmType
structThunk = LMAlias ("struct.Thunk", llvmTypeOfStruct ddcThunk)

pStructThunk :: LlvmType
pStructThunk = pLift structThunk

--------------------------------------------------------------------------------

ddcData :: LlvmStructDesc
ddcData
 =	mkLlvmStructDesc "Data"
		[ AField "tag" i32			-- tag
		, AField "arity" i32			-- arity
		, AField "args" (LMArray 0 pObj) ]	-- array of arguments


structData :: LlvmType
structData = LMAlias ("struct.Data", llvmTypeOfStruct ddcData)

pStructData :: LlvmType
pStructData = pLift structData

--------------------------------------------------------------------------------

ddcDataRS :: LlvmStructDesc
ddcDataRS
 =	mkLlvmStructDesc "DataRS"
		[ AField "tag" i32			-- tag
		, AField "payload" (LMArray 0 i8) ]	-- data


structDataRS :: LlvmType
structDataRS = LMAlias ("struct.DataRS", llvmTypeOfStruct ddcData)

pStructDataRS :: LlvmType
pStructDataRS = pLift structDataRS

--------------------------------------------------------------------------------

objModeForward :: Int
objModeForward	= 0x00

objModeFixed :: Int
objModeFixed	= 0x01

objModeDataRS :: Int
objModeDataRS = 0x03
