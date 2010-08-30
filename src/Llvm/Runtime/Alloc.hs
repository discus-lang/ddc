module Llvm.Runtime.Alloc
	( allocate
	{- , allocThunk -} )
where

import Util

import DDC.Main.Error

import Llvm
import LlvmM
import Llvm.Runtime.Data
import Llvm.Util

stage = "Llvm.Runtime.Alloc"


-- | Generate LLVM code that can be inlined to allocate the given number of
-- bytes and return a pointer of the specified type.
-- The generated code will always allocate heap objects aligned to 8 byte
-- boundaries and panics if asked to allocate zero or less bytes.
allocate :: Int -> String -> LlvmM LlvmVar
allocate bytes name
 = do	ptr	<- lift $ newUniqueNamedReg name pObj
	r0	<- lift $ newUniqueNamedReg "r0" pChar
	r1	<- lift $ newUniqueNamedReg "r1" pChar
	r2	<- lift $ newUniqueNamedReg "r2" pChar
	r3	<- lift $ newUniqueNamedReg "r3" i1
	r4	<- lift $ newUniqueNamedReg "r4" pChar
	r5	<- lift $ newUniqueNamedReg "r5" (pLift i32)
	r6	<- lift $ newUniqueNamedReg "r6" pChar
	r7	<- lift $ newUniqueNamedReg "r7" pChar
	r8	<- lift $ newUniqueNamedReg "r8" (pLift i32)
	pre	<- lift $ newUniqueNamedReg "pre" pChar

	entry	<- lift $ newUniqueLabel "allocate"
	bb	<- lift $ newUniqueLabel "bb"
	bb1	<- lift $ newUniqueLabel "bb1"

	let count = i32LitVar $ roundUpBytes bytes

	addBlock
		[ Comment ["allocate " ++ show bytes]
		, Branch entry
		, MkLabel (uniqueOfLlvmVar entry)
		, Assignment r0 (Load ddcHeapPtr)
		, Assignment r1 (GetElemPtr True r0 [count])
		, Assignment r2 (Load ddcHeapMax)
		, Assignment r3 (Compare LM_CMP_Ugt r1 r2)
		, BranchIf r3 bb bb1

		, MkLabel (uniqueOfLlvmVar bb)
		, Expr (Call StdCall (LMGlobalVar "_allocCollect" (LMFunction allocCollect) External Nothing Nothing True) [count] [])
		, Assignment pre (Load ddcHeapPtr)
		, Branch bb1

		, MkLabel (uniqueOfLlvmVar bb1)
		, Assignment r4 (Phi pChar [(pre, bb), (r0 , entry)])

		, Assignment r5 (Cast LM_Bitcast r4 (pLift i32))
		, Assignment r6 (GetElemPtr True r4 [count])
		, Store r6 ddcHeapPtr
		, Assignment ptr (Cast LM_Bitcast r4 (getVarType ptr))
		]
	return	ptr


-- static inline Obj*	_allocThunk	(FunPtr	func,	UInt airity,	UInt args);

{-
allocThunk :: a -> Int -> Int -> LlvmM LlvmVar
allocThunk funPtr airity args
 =
-}



-- | Round up to a multiple of 8.
roundUpBytes :: Int -> Int
roundUpBytes n
 | n <= 0
 = panic stage $ "allocate with " ++ show n

 | mod n 8 == 0
 = n

 | otherwise
 = n + (8 - mod n 8)


