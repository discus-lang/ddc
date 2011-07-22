{-# OPTIONS -fwarn-unused-imports -fno-warn-type-defaults #-}
module Llvm.Runtime.Alloc
	( allocate
	, allocateVarSize
	, allocThunk
	, allocData
	, allocDataM
	, allocDataR
	, allocDataRSbySize
	, allocDataRSbyType )
where

import DDC.Main.Error

import Llvm
import LlvmM
import Llvm.Runtime.Object
import Llvm.Runtime.Data
import Llvm.Runtime.Struct
import Llvm.Runtime.Tags
import Llvm.Util

import Control.Monad		(when)

stage = "Llvm.Runtime.Alloc"


-- | Generate LLVM code that can be inlined to allocate the given number of
-- bytes and return a pointer of the specified type.
-- The generated code will always allocate heap objects aligned to 8 byte
-- boundaries and panics if asked to allocate zero or less bytes.
allocate :: Int -> String -> LlvmType -> LlvmM LlvmVar
allocate bytes name typ
 = 	allocateVarSize (i32LitVar $ roundUpBytes bytes) name typ


allocateVarSize :: LlvmVar -> String -> LlvmType -> LlvmM LlvmVar
allocateVarSize size name typ
 = do	addGlobalFuncDecl allocCollect
	ptr	<- newUniqueNamedReg name typ
	r0	<- newUniqueNamedReg "r0" pChar
	r1	<- newUniqueNamedReg "r1" pChar
	r2	<- newUniqueNamedReg "r2" pChar
	r3	<- newUniqueNamedReg "r3" i1
	r4	<- newUniqueNamedReg "r4" pChar
	r5	<- newUniqueNamedReg "r5" (pLift i32)
	r6	<- newUniqueNamedReg "r6" pChar
	r7	<- newUniqueNamedReg "r7" pChar
	r8	<- newUniqueNamedReg "r8" (pLift i32)
	pre	<- newUniqueNamedReg "pre" pChar

	entry	<- newUniqueLabel "allocate"
	bb	<- newUniqueLabel "bb"
	bb1	<- newUniqueLabel "bb1"

	addBlock
		[ Comment ["allocate " ++ show size ++ " bytes"]
		, Branch entry
		, MkLabel (uniqueOfLlvmVar entry)
		, Assignment r0 (Load ddcHeapPtr)
		, Assignment r1 (GetElemPtr True r0 [size])
		, Assignment r2 (Load ddcHeapMax)
		, Assignment r3 (Compare LM_CMP_Ugt r1 r2)
		, BranchIf r3 bb bb1

		, MkLabel (uniqueOfLlvmVar bb)
		, Expr (Call StdCall (LMGlobalVar "_allocCollect" (LMFunction allocCollect) External Nothing Nothing True) [size] [])
		, Assignment pre (Load ddcHeapPtr)
		, Branch bb1

		, MkLabel (uniqueOfLlvmVar bb1)
		, Assignment r4 (Phi pChar [(pre, bb), (r0 , entry)])

		, Assignment r5 (Cast LM_Bitcast r4 (pLift i32))
		, Assignment r6 (GetElemPtr True r4 [size])
		, Store r6 ddcHeapPtr
		]
	if typ == pChar
	  then		return r2
	  else do	addBlock [ Assignment ptr (Cast LM_Bitcast r4 typ) ]
			return	ptr


allocThunk :: LlvmVar -> Int -> Int -> LlvmM LlvmVar
allocThunk funvar arity argc
 = do	addAlias	("struct.Thunk", llvmTypeOfStruct ddcThunk)
	let size	= sizeOfLlvmType structThunk + arity * sizeOfLlvmType pObj
	addComment	$ "allocThunk " ++ getName funvar ++ " " ++ show arity ++ " " ++ show argc

	pThunk		<- allocate size "pThunk" pStructThunk

	storeStructRegValue ddcThunk pThunk "tag" tagFixedThunk

	pFunSrc		<- newUniqueNamedReg "pFunSrc" genericFunPtrType
	addBlock	[ Assignment pFunSrc (Cast LM_Bitcast funvar genericFunPtrType) ]
	storeStructRegValue ddcThunk pThunk "funptr" pFunSrc

	storeStructRegValue ddcThunk pThunk "arity" (i32LitVar arity)
	storeStructRegValue ddcThunk pThunk "argc" (i32LitVar argc)

	when (argc > 0)
	  $ addComment "Thunk's args array does not need to be initialized."

	ret		<- newUniqueNamedReg "allocated.thunk" pObj
	addBlock	[ Assignment ret (Cast LM_Bitcast pThunk pObj) ]
	return		ret


allocData :: Int -> Int -> LlvmM LlvmVar
allocData tag arity
 = do	addAlias	("struct.Data", llvmTypeOfStruct ddcData)
	let size	= sizeOfLlvmType structData + arity * sizeOfLlvmType pObj
	addComment	$ "allocData " ++ show tag ++ " " ++ show arity

	pData		<- allocate size "pData" pStructData

	storeStructRegValue ddcData pData "tag" (tagData tag)
	storeStructRegValue ddcData pData "arity" (i32LitVar arity)

	ret		<- newUniqueNamedReg "allocated.data" pObj
	addBlock	[ Assignment ret (Cast LM_Bitcast pData pObj) ]
	return		ret


allocDataM :: Int -> Int -> Int -> LlvmM LlvmVar
allocDataM tag dataSize ptrCount
 = do	addAlias	("struct.DataM", llvmTypeOfStruct ddcData)
	let size	= sizeOfLlvmType structDataM + ptrCount * sizeOfLlvmType pObj + roundUpBytes dataSize
	addComment	$ "allocDataM " ++ show tag ++ " " ++ show dataSize ++ " " ++ show ptrCount

	pDataM		<- allocate size "pDataM" pStructDataM

	storeStructRegValue ddcDataM pDataM "tag" (tagDataM tag)
	storeStructRegValue ddcDataM pDataM "size" (i32LitVar size)
	storeStructRegValue ddcDataM pDataM "ptrCount" (i32LitVar 0)

	ret		<- newUniqueNamedReg "allocated.data" pObj
	addBlock	[ Assignment ret (Cast LM_Bitcast pDataM pObj) ]
	return		ret


allocDataR :: Int -> LlvmVar -> LlvmM LlvmVar
allocDataR tag dataSize
 = do	addAlias	("struct.DataR", llvmTypeOfStruct ddcDataR)

	let size	= roundUpBytes (sizeOfLlvmType structDataR)

	tsize		<- newUniqueNamedReg "tsize" i32
	ret		<- newUniqueNamedReg "allocated.DataR" pObj

	addComment	$ "allocDataR " ++ show tag ++ " " ++ show size

	addBlock	[ Assignment tsize (LlvmOp LM_MO_Add dataSize (i32LitVar size)) ]

	pDataR		<- allocateVarSize tsize "pDataR" pStructDataR

	storeStructRegValue ddcDataR pDataR "tag" (tagDataR tag)
	storeStructRegValue ddcDataR pDataR "size" tsize

	addBlock	[ Assignment ret (Cast LM_Bitcast pDataR pObj) ]
	return		ret


allocDataRSbyType :: Int -> Int -> LlvmType -> LlvmM (LlvmVar, LlvmVar)
allocDataRSbyType tag dataSize pType
 = do	addAlias	("struct.DataRS", llvmTypeOfStruct ddcDataRS)

	let size	= roundUpBytes (sizeOfLlvmType structDataRS + dataSize)
	let dataWords	= dataSize `div` 4

	addComment	$ "allocDataRSbyType " ++ show tag ++ " " ++ show dataSize ++ " " ++ show pType

	pDataRS		<- allocate size "pDataRS" pStructDataRS

	storeStructRegValue ddcDataRS pDataRS "tag" (tagDataRS tag dataWords)

	ptr		<- newUniqueReg pChar
	payload		<- newUniqueNamedReg "payload" pType
	ret		<- newUniqueNamedReg "allocated.DataRS" pObj

	addBlock	[ Assignment ptr (GetElemPtr True pDataRS [i32LitVar 0, i32LitVar $ fst $ structFieldLookup ddcDataRS "payload", i32LitVar 0 ])
			, Assignment payload (Cast LM_Bitcast ptr pType)
			, Assignment ret (Cast LM_Bitcast pDataRS pObj) ]
	return		(ret, payload)


allocDataRSbySize :: Int -> Int -> LlvmM LlvmVar
allocDataRSbySize tag dataSize
 = do	addAlias	("struct.DataRS", llvmTypeOfStruct ddcDataRS)

	let size	= roundUpBytes (sizeOfLlvmType structDataRS + dataSize)
	let dataWords	= dataSize `div` 4

	addComment	$ "allocDataRSbySize " ++ show tag ++ " " ++ show dataSize

	pDataRS		<- allocate size "pDataRS" pStructDataRS
	ret		<- newUniqueNamedReg "allocated.DataRS" pObj

	storeStructRegValue ddcDataRS pDataRS "tag" (tagDataRS tag dataWords)

	addBlock	[ Assignment ret (Cast LM_Bitcast pDataRS pObj) ]
	return		ret

--------------------------------------------------------------------------------
-- Store the given value in named field of the struct described by desc and
-- referred to by the LlvmVar struct.
storeStructRegValue :: LlvmStructDesc -> LlvmVar -> String -> LlvmVar -> LlvmM ()
storeStructRegValue desc struct field value
 = do	let (indx, typ)	= structFieldLookup desc field
	ptag		<- newUniqueNamedReg field (pLift typ)
	addBlock	[ Assignment ptag (GetElemPtr True struct [i32LitVar 0, i32LitVar indx])
			, Store value ptag ]


-- | Round up to a multiple of 8.
roundUpBytes :: Int -> Int
roundUpBytes n
 | n <= 0
 = panic stage $ "allocate with " ++ show n

 | mod n 8 == 0
 = n

 | otherwise
 = n + (8 - mod n 8)


