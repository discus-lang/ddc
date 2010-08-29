{-# OPTIONS -fno-warn-type-defaults #-}
module Llvm.Runtime.Boxing
	( boxAny	, unboxAny

	, boxEnum
	, boxInt32	, unboxInt32
	, boxInt64
	, boxFloat32
	, boxFloat64 )
where

import Util

import DDC.Main.Error

import Llvm
import LlvmM
import Llvm.Runtime.Alloc
import Llvm.Runtime.Tags
import Llvm.Util


stage = "Llvm.Runtime.Boxing"


boxAny :: LlvmVar -> LlvmM LlvmVar
boxAny any
 = case getVarType any of
	LMInt 1		-> boxEnum any
	LMInt 32	-> boxInt32 any
	_		-> panic stage $ "boxAny " ++ show (getVarType any)


unboxAny :: LlvmType -> LlvmVar -> LlvmM LlvmVar
unboxAny anyType any
 = case anyType of
	LMInt 32	-> unboxInt32 any
	_		-> panic stage $ "unboxAny " ++ show anyType

--------------------------------------------------------------------------------

boxInt32 :: LlvmVar -> LlvmM LlvmVar
boxInt32 int32
 = do	iptr0	<- lift $ newUniqueNamedReg "iptr0" (pLift i32)
	iptr1	<- lift $ newUniqueNamedReg "iptr1" (pLift i32)
	objptr	<- allocate 8 "boxed"
	addBlock
		[ Comment [ "boxInt32 (" ++ show int32 ++ ")" ]
		, Assignment iptr0 (Cast LM_Bitcast objptr (pLift i32))
		, Store (tagDataRS 1) iptr0
		, Assignment iptr1 (GetElemPtr True iptr0 [llvmWordLitVar 1])
		, Store int32 iptr1
		]
	return	objptr


unboxInt32 :: LlvmVar -> LlvmM LlvmVar
unboxInt32 objptr
 | getVarType objptr == pObj
 = do	int32	<- lift $ newUniqueReg i32
	iptr0	<- lift $ newUniqueNamedReg "iptr0" (pLift i32)
	iptr1	<- lift $ newUniqueNamedReg "iptr1" (pLift i32)
	addBlock
		[ Comment [ show int32 ++ " = unboxInt32 (" ++ show objptr ++ ")" ]
		, Assignment iptr0 (GetElemPtr True objptr [llvmWordLitVar 0, i32LitVar 0])
		, Assignment iptr1 (GetElemPtr True iptr0 [llvmWordLitVar 1])
		, Assignment int32 (Load iptr1)
		]
	return	int32

unboxInt32 objptr
 | getVarType objptr == LMPointer ppObj
 = do	optr0	<- lift $ newUniqueNamedReg "optr0" ppObj
	optr1	<- lift $ newUniqueNamedReg "optr1" pObj
	addBlock
		[ Comment [ show optr1 ++ " = unboxInt32 (" ++ show objptr ++ ")" ]
		, Assignment optr0 (Load objptr)
		, Assignment optr1 (Load optr0)
		]
	unboxInt32 optr1

 | otherwise
 =	panic stage $ "unboxInt32 (" ++ show objptr ++ ")"

--------------------------------------------------------------------------------

boxEnum :: LlvmVar -> LlvmM LlvmVar
boxEnum v@(LMLocalVar u (LMInt 1))
 = do	int32	<- lift $ newUniqueNamedReg "int32" i32
	shifted	<- lift $ newUniqueNamedReg "shifted" i32
	tag	<- lift $ newUniqueNamedReg "tag" i32
	iptr0	<- lift $ newUniqueNamedReg "iptr0" (pLift i32)
	iptr1	<- lift $ newUniqueNamedReg "iptr1" (pLift i32)
	objptr	<- allocate 8 "boxed"
	addBlock
		[ Comment [ "boxEnum (" ++ show v ++ ")" ]
		, Assignment int32 (Cast LM_Zext v i32)
		, Assignment shifted (LlvmOp LM_MO_Shl int32 (i32LitVar 8))
		, Assignment tag (LlvmOp LM_MO_Or shifted tagData)
		, Assignment iptr0 (Cast LM_Bitcast objptr (pLift i32))
		, Store tag iptr0
		, Assignment iptr1 (GetElemPtr True iptr0 [llvmWordLitVar 1])
		, Store (i32LitVar 0) iptr1
		]
	return	objptr

boxEnum any
 = panic stage $ "boxEnumX " ++ show any

--------------------------------------------------------------------------------

boxInt64 :: LlvmVar -> LlvmM LlvmVar
boxInt64 i
 = panic stage "unimplemented"

boxFloat32 :: LlvmVar -> LlvmM LlvmVar
boxFloat32 f
 = panic stage "unimplemented"

boxFloat64 :: LlvmVar -> LlvmM LlvmVar
boxFloat64 f
 = panic stage "unimplemented"

