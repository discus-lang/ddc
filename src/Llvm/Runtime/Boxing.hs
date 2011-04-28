{-# OPTIONS -fno-warn-type-defaults -cpp #-}
module Llvm.Runtime.Boxing
	( boxAny	, unboxAny

	, boxEnum
	, boxLit
	, boxInt32	, unboxInt32
	, boxInt64
	, boxFloat32
	, boxFloat64 )
where

import DDC.Base.DataFormat
import DDC.Base.Literal
import DDC.Main.Error

import Llvm
import LlvmM
import Llvm.Runtime.Alloc
import Llvm.Runtime.Object
import Llvm.Runtime.Struct
import Llvm.Runtime.Tags
import Llvm.Util


stage = "Llvm.Runtime.Boxing"


boxAny :: LlvmVar -> LlvmM LlvmVar
boxAny any
 = case getVarType any of
	LMInt 1			-> boxBool any
	LMInt 32		-> boxInt32 any
	LMInt 64		-> boxInt64 any
	LMFloat			-> boxFloat32 any
	LMDouble		-> boxFloat64 any
	-- LMArray _ (LMInt 8)	-> boxString any
	_			-> panic stage $ "boxAny " ++ show  any


unboxAny :: LlvmType -> LlvmVar -> LlvmM LlvmVar
unboxAny anyType any
 = case anyType of
	LMInt 1		-> unboxBool any
	LMInt 32	-> unboxInt32 any
	LMInt 64	-> unboxInt64 any
	LMFloat		-> unboxFloat32 any
	LMDouble	-> unboxFloat64 any
	_		-> panic stage $ "unboxAny " ++ show anyType

--------------------------------------------------------------------------------

boxInt32 :: LlvmVar -> LlvmM LlvmVar
boxInt32 int32
 = do	iptr0	<- newUniqueNamedReg "iptr0" (pLift i32)
	iptr1	<- newUniqueNamedReg "iptr1" (pLift i32)
	objptr	<- allocate 8 "boxed" pObj
	addBlock
		[ Assignment iptr0 (Cast LM_Bitcast objptr (pLift i32))
		, Store (tagDataRS 1) iptr0
		, Assignment iptr1 (GetElemPtr True iptr0 [llvmWordLitVar 1])
		, Store int32 iptr1
		]
	return	objptr


unboxInt32 :: LlvmVar -> LlvmM LlvmVar
unboxInt32 objptr
 | getVarType objptr == pObj
 = do	int32	<- newUniqueReg i32
	iptr0	<- newUniqueNamedReg "iptr0" (pLift i32)
	iptr1	<- newUniqueNamedReg "iptr1" (pLift i32)
	addBlock
		[ Assignment iptr0 (GetElemPtr True objptr [llvmWordLitVar 0, i32LitVar 0])
		, Assignment iptr1 (GetElemPtr True iptr0 [llvmWordLitVar 1])
		, Assignment int32 (Load iptr1)
		]
	return	int32

unboxInt32 objptr
 | getVarType objptr == LMPointer ppObj
 = do	optr0	<- newUniqueNamedReg "optr0" ppObj
	optr1	<- newUniqueNamedReg "optr1" pObj
	addBlock
		[ Assignment optr0 (Load objptr)
		, Assignment optr1 (Load optr0)
		]
	unboxInt32 optr1

 | otherwise
 =	panic stage $ "unboxInt32 (" ++ show objptr ++ ")"

--------------------------------------------------------------------------------

boxBool :: LlvmVar -> LlvmM LlvmVar
boxBool bool
 | getVarType bool == i1
 = do	int32		<- newUniqueNamedReg "i32.of.i1" i32
	addBlock	[ Assignment int32 (Cast LM_Zext bool i32) ]
	boxEnum		int32

 | otherwise
 =	boxEnum		bool


unboxBool :: LlvmVar -> LlvmM LlvmVar
unboxBool objptr
 | getVarType objptr == pObj
 = do	int32		<- unboxEnum objptr
	bool		<- newUniqueReg i1
	addBlock	[ Assignment bool (Compare LM_CMP_Ne int32 (i32LitVar 0)) ]
	return		bool

--------------------------------------------------------------------------------

boxEnum :: LlvmVar -> LlvmM LlvmVar
boxEnum enum
 = do	shifted	<- newUniqueNamedReg "shifted" i32
	tag	<- newUniqueNamedReg "tag" i32
	iptr0	<- newUniqueNamedReg "iptr0" (pLift i32)
	iptr1	<- newUniqueNamedReg "iptr1" (pLift i32)
	objptr	<- allocate 8 "boxed" pObj
	addBlock
		[ Assignment shifted (LlvmOp LM_MO_Shl enum (i32LitVar 8))
		, Assignment tag (LlvmOp LM_MO_Or shifted (tagData 0))
		, Assignment iptr0 (Cast LM_Bitcast objptr (pLift i32))
		, Store tag iptr0
		, Assignment iptr1 (GetElemPtr True iptr0 [llvmWordLitVar 1])
		, Store (i32LitVar 0) iptr1
		]
	return	objptr

unboxEnum :: LlvmVar -> LlvmM LlvmVar
unboxEnum objptr
 | getVarType objptr == pObj
 = do	iptr0	<- newUniqueNamedReg "iptr0" (pLift i32)
	int32	<- newUniqueReg i32
	enum	<- newUniqueReg i32
	addBlock
		[ Assignment iptr0 (GetElemPtr True objptr [llvmWordLitVar 0, i32LitVar 0])
		, Assignment int32 (Load iptr0)
		, Assignment enum (LlvmOp LM_MO_LShr int32 (i32LitVar 8)) ]
	return	enum

 | otherwise
 =	panic stage $ "unboxBool (" ++ show objptr ++ ")"

--------------------------------------------------------------------------------

boxLit :: LiteralFmt -> LlvmM LlvmVar
boxLit lit@(LiteralFmt _ (UnboxedBits _))
 =	boxAny $ llvmVarOfLit lit


boxLit lit = panic stage $ "boxLit (" ++ show __LINE__ ++ ") " ++ show lit

--------------------------------------------------------------------------------

boxFloat32 :: LlvmVar -> LlvmM LlvmVar
boxFloat32 f32
 = do	iptr0	<- newUniqueNamedReg "iptr0" (pLift i32)
	iptr1	<- newUniqueNamedReg "iptr1" (pLift i32)
	fptr	<- newUniqueNamedReg "fptr" (pLift LMFloat)
	objptr	<- allocate 8 "boxed" pObj
	addBlock
		[ Assignment iptr0 (Cast LM_Bitcast objptr (pLift i32))
		, Store (tagDataRS 1) iptr0
		, Assignment iptr1 (GetElemPtr True iptr0 [llvmWordLitVar 1])
		, Assignment fptr (Cast LM_Bitcast iptr1 (pLift LMFloat))
		, Store f32 fptr
		]
	return	objptr

unboxFloat32 :: LlvmVar -> LlvmM LlvmVar
unboxFloat32 objptr
 | getVarType objptr == pObj
 = do	f32	<- newUniqueReg LMFloat
	iptr0	<- newUniqueNamedReg "iptr0" (pLift i32)
	iptr1	<- newUniqueNamedReg "iptr1" (pLift i32)
	fptr	<- newUniqueNamedReg "fptr" (pLift LMFloat)
	addBlock
		[ Assignment iptr0 (GetElemPtr True objptr [llvmWordLitVar 0, i32LitVar 0])
		, Assignment iptr1 (GetElemPtr True iptr0 [llvmWordLitVar 1])
		, Assignment fptr (Cast LM_Bitcast iptr1 (pLift LMFloat))
		, Assignment f32 (Load fptr)
		]
	return	f32

--------------------------------------------------------------------------------

boxInt64 :: LlvmVar -> LlvmM LlvmVar
boxInt64 int64
 = do	(objptr, dptr)
		<- allocDataRSbyType (sizeOfLlvmType i64) (pLift i64)
	addBlock
		[ Store int64 dptr ]
	return	objptr


unboxInt64 :: LlvmVar -> LlvmM LlvmVar
unboxInt64 objptr
 | getVarType objptr == pObj
 = do	pDataRS	<- newUniqueReg pStructDataRS
	ptr	<- newUniqueReg pChar
	i64ptr	<- newUniqueReg (LMPointer i64)
	int64	<- newUniqueReg i64
	addBlock
		[ Assignment pDataRS (Cast LM_Bitcast objptr pStructDataRS)
		, Assignment ptr (GetElemPtr True pDataRS [i32LitVar 0, i32LitVar $ fst $ structFieldLookup ddcDataRS "payload", i32LitVar 0 ])
		, Assignment i64ptr (Cast LM_Bitcast ptr (LMPointer i64))
		, Assignment int64 (Load i64ptr)
		]
	return	int64

--------------------------------------------------------------------------------

boxFloat64 :: LlvmVar -> LlvmM LlvmVar
boxFloat64 f64
 = do	(objptr, dptr)
		<- allocDataRSbyType (sizeOfLlvmType LMDouble) (pLift LMDouble)
	addBlock
		[ Store f64 dptr ]
	return	objptr

unboxFloat64 :: LlvmVar -> LlvmM LlvmVar
unboxFloat64  objptr
 | getVarType objptr == pObj
 = do	pDataRS	<- newUniqueReg pStructDataRS
	ptr	<- newUniqueReg pChar
	f64ptr	<- newUniqueReg (LMPointer LMDouble)
	f64	<- newUniqueReg LMDouble
	addBlock
		[ Assignment pDataRS (Cast LM_Bitcast objptr pStructDataRS)
		, Assignment ptr (GetElemPtr True pDataRS [i32LitVar 0, i32LitVar $ fst $ structFieldLookup ddcDataRS "payload", i32LitVar 0 ])
		, Assignment f64ptr (Cast LM_Bitcast ptr (LMPointer LMDouble))
		, Assignment f64 (Load f64ptr)
		]
	return	f64

