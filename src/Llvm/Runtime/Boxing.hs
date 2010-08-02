module Llvm.Runtime.Boxing
	( boxInt32	, unboxInt32
	, boxInt64
	, boxFloat32
	, boxFloat64 )
where

import DDC.Main.Error

import Llvm
import Llvm.Runtime.Alloc
import Llvm.Util


stage = "Llvm.Runtime.Boxing"


boxInt32 :: LlvmVar -> LlvmVar -> IO [LlvmStatement]
boxInt32 int32 objptr
 = do	iptr0		<- newUniqueNamedReg "iptr0" (pLift i32)
	iptr1		<- newUniqueNamedReg "iptr1" (pLift i32)
	
	allocCode	<- allocate 8 objptr
	return $ 
		allocCode
		++ [ Comment ["boxIn32 (" ++ show int32 ++ ")" ]
		   , Assignment iptr0 (Cast LM_Bitcast objptr (pLift i32))
		   , Store (i32LitVar (19 :: Int)) iptr0
		   , Assignment iptr1 (GetElemPtr True iptr0 [llvmWordLitVar (1 :: Int)])
		   , Store int32 iptr1
		   ]

unboxInt32 :: LlvmVar -> LlvmVar -> IO [LlvmStatement]
unboxInt32 objptr int32
 = do	iptr0		<- newUniqueNamedReg "iptr0" (pLift i32)
	iptr1		<- newUniqueNamedReg "iptr1" (pLift i32)

	return $ [ Comment ["unboxIn32 (" ++ show objptr ++ ")" ]
		 , Assignment iptr0 (Cast LM_Bitcast objptr (pLift i32))
		 , Assignment iptr1 (GetElemPtr True iptr0 [llvmWordLitVar (1 :: Int)])
		 , Assignment int32 (Load iptr1)
		 ]


boxInt64 :: LlvmVar -> IO (LlvmVar, [LlvmStatement])
boxInt64 i
 = panic stage "unimplemented"

boxFloat32 :: LlvmVar -> IO (LlvmVar, [LlvmStatement])
boxFloat32 f
 = panic stage "unimplemented"

boxFloat64 :: LlvmVar -> IO (LlvmVar, [LlvmStatement])
boxFloat64 f
 = panic stage "unimplemented"

