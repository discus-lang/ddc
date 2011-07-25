{-# OPTIONS -fwarn-unused-imports -fno-warn-type-defaults #-}

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
	, ddcDataM
	, ddcDataR
	, ddcDataAUI
	, ddcDataRS
	, ddcSuspIndir
	, ddcFile

	, structObj
	, structThunk
	, structData
	, structDataM
	, structDataR
	, structDataAUI
	, structDataRS
	, structSuspIndir
	, structFile

	, pObj
	, pStructThunk
	, pStructData
	, pStructDataM
	, pStructDataR
	, pStructDataAUI
	, pStructDataRS
	, pStructSuspIndir
	, pStructFile

	, nullObj
	, ppObj )
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

ddcDataM :: LlvmStructDesc
ddcDataM
 =	mkLlvmStructDesc "DataM"
		[ AField "tag" i32
		, AField "padding" i32
		, AField "size" i32
		, AField "ptrCount" i32
		, AField "payload" (LMArray 0 i8) ]


structDataM :: LlvmType
structDataM = LMAlias ("struct.DataM", llvmTypeOfStruct ddcData)

pStructDataM :: LlvmType
pStructDataM = pLift structDataM

--------------------------------------------------------------------------------

ddcDataR :: LlvmStructDesc
ddcDataR
 =	mkLlvmStructDesc "DataR"
		[ AField "tag" i32			-- tag
		, AField "size" i32
		, AField "payload" (LMArray 0 i8) ]	-- data

structDataR :: LlvmType
structDataR = LMAlias ("struct.DataR", llvmTypeOfStruct ddcDataR)

pStructDataR :: LlvmType
pStructDataR = pLift structDataR

--------------------------------------------------------------------------------

ddcDataAUI :: LlvmStructDesc
ddcDataAUI
 =	mkLlvmStructDesc "DataAUI"
		[ AField "tag" i32			-- tag
		, AField "size" i32
		, AField "elemCount" i32
		, AField "elem" (LMArray 0 i32) ]	-- data

structDataAUI :: LlvmType
structDataAUI = LMAlias ("struct.DataAUI", llvmTypeOfStruct ddcDataAUI)

pStructDataAUI :: LlvmType
pStructDataAUI = pLift structDataAUI

--------------------------------------------------------------------------------

ddcDataRS :: LlvmStructDesc
ddcDataRS
 =	mkLlvmStructDesc "DataRS"
		[ AField "tag" i32			-- tag
		, AField "payload" (LMArray 0 i8) ]	-- data


structDataRS :: LlvmType
structDataRS = LMAlias ("struct.DataRS", llvmTypeOfStruct ddcDataRS)

pStructDataRS :: LlvmType
pStructDataRS = pLift structDataRS

--------------------------------------------------------------------------------

ddcSuspIndir :: LlvmStructDesc
ddcSuspIndir
 =	mkLlvmStructDesc "SuspIndir"
		[ AField "tag" i32
		, AField "arity" i32
		, AField "obj" pObj
		, AField "a" (LMArray 0 pObj) ]

structSuspIndir :: LlvmType
structSuspIndir = LMAlias ("struct.SuspIndir", llvmTypeOfStruct ddcSuspIndir)

pStructSuspIndir :: LlvmType
pStructSuspIndir = pLift structSuspIndir

--------------------------------------------------------------------------------
-- | We need a structFile as a place holder for the C stdio's FILE pointer.

ddcFile :: LlvmType
ddcFile = LMStruct [ LMInt 666 ]

structFile :: LlvmType
structFile = LMAlias ("struct.File", ddcFile)

pStructFile :: LlvmType
pStructFile = structFile
