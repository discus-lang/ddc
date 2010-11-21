{-# OPTIONS -fwarn-unused-imports -fno-warn-type-defaults -cpp #-}

-- | Functions for defining DDC structs so that they can interact with the C
-- runtime. Struct definitions in the file need to be kept in sync with the
-- ones in runtime/Object.h.

module Llvm.Runtime.Object
	( genericFunPtrType
	, ptrAlign

	, ddcObj
	, ddcThunk
	, nullObj

	, pObj
	, ppObj
	, structObj
	, structThunk
	, pStructThunk )
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

