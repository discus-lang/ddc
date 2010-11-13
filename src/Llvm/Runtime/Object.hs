{-# OPTIONS -fwarn-unused-imports -fno-warn-type-defaults -cpp #-}

-- | Functions for defining DDC structs so that they can interact with the C
-- runtime.
module Llvm.Runtime.Object
	( sizeOfLlvmType
	, offsetOfIndex

	, genericFunPtrType
	, ptrAlign

	, ddcObj
	, ddcThunk
	, nullObj

	, pObj
	, ppObj
	, structObj
	, pThunk
	, structThunk )
where

import DDC.Main.Error

import Llvm

import qualified Config.Config		as Config


stage = "Llvm.Runtime.Object"


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
genericFunPtrType = LMPointer (LMFunction (LlvmFunctionDecl "generic.function.ptr.type" Internal CC_Ccc LMVoid FixedArgs [] ptrAlign))


ptrAlign :: Maybe Int
ptrAlign = Just Config.pointerBytes

--------------------------------------------------------------------------------
-- | A structure can be defined as a list of StructFields with optional padding
-- to force the right alignment. Padding is only added if its needed to make the
-- next field start at the required offset.

{-

data StructField
	= Field String LlvmType		-- Field name and type.
--	| PadTo16			-- Pad next field to 16 bit offset.
--	| PadTo32			-- Pad next field to 32 bit offset.
	| PadTo64			-- Pad next field to 64 bit offset.



defThunk :: Int -> [StructField]
defThunk argc
 =	[ Field "tag" i32			-- tag
	, PadTo64
	, Field "funptr" genericFunPtrType	-- function pointer
	, Field "arity" i32			-- arity
	, Field "argc" i32			-- arg count
	, PadTo64
	, Field "args" (LMArray argc pObj) ]	-- array of arguments



llvmTypeOfFields :: [StructField] -> LlvmType
llvmTypeOfFields fields

-}


thunk32 :: LlvmType
thunk32
 = LMStruct 	[ i32			-- tag
		, genericFunPtrType	-- function pointer
		, i32			-- arity
		, i32			-- args
		, LMArray 0 pObj	-- Pointer to arguments
		]

thunk64 :: LlvmType
thunk64
 = LMStruct 	[ i32			-- tag
		, genericFunPtrType 	-- function pointer
		, i32			-- arity
		, i32			-- args
		, LMArray 0 pObj	-- Pointer to arguments
		]

ddcThunk :: LlvmType
ddcThunk
 = case Config.pointerBytes of
	4 -> thunk32
	8 -> thunk64
	_ -> panic stage $ "Config.pointerBytes == " ++ show Config.pointerBytes

structThunk :: LlvmType
structThunk = LMAlias ("struct.Thunk", ddcThunk)

pThunk :: LlvmType
pThunk = pLift structThunk

sizeOfLlvmType :: LlvmType -> Int
sizeOfLlvmType t
 = case t of
	LMInt bits	-> div bits 8
	LMFloat		-> 4
	LMDouble	-> 8
	LMFloat80	-> 10
	LMFloat128	-> 16
	LMPointer _	-> Config.pointerBytes
	LMArray n t	-> n * sizeOfLlvmType t
	LMLabel		-> panic stage $ "sizeOfLlvmType LMLabel"
	LMVoid		-> panic stage $ "sizeOfLlvmType LMVoid"
	LMStruct t	-> sum $ map sizeOfLlvmType t
	LMAlias (_, t)	-> sizeOfLlvmType t

offsetOfIndex :: LlvmType -> Int -> Int
offsetOfIndex _ 0 = 0
offsetOfIndex typ i
 | i > 0
 = case typ of
	LMLabel		-> panic stage $ "offsetOfIndex LMLabel"
	LMVoid		-> panic stage $ "offsetOfIndex LMVoid"
	LMStruct t	-> sum $ take i $ map sizeOfLlvmType t
	LMAlias (_, t)	-> offsetOfIndex t i
	_		-> 0


