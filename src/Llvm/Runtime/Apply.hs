{-# OPTIONS -Wall -Werror -fwarn-unused-imports #-}
module Llvm.Runtime.Apply
	( applyN )
where


import DDC.Main.Error

import Llvm
import LlvmM
import Llvm.Util

stage :: String
stage = "Llvm.Runtime.Apply"

-- LLVM declarations for functions in runtime/Apply.(c|h).

apply1FD :: LlvmFunctionDecl
apply1FD = LlvmFunctionDecl "_apply1" External CC_Ccc pObj FixedArgs [(pObj, []), (pObj, [])] ptrAlign

apply2FD :: LlvmFunctionDecl
apply2FD = LlvmFunctionDecl "_apply2" External CC_Ccc pObj FixedArgs [(pObj, []), (pObj, []), (pObj, [])] ptrAlign

apply3FD :: LlvmFunctionDecl
apply3FD = LlvmFunctionDecl "_apply3" External CC_Ccc pObj FixedArgs [(pObj, []), (pObj, []), (pObj, []), (pObj, [])] ptrAlign

apply4FD :: LlvmFunctionDecl
apply4FD = LlvmFunctionDecl "_apply4" External CC_Ccc pObj FixedArgs [(pObj, []), (pObj, []), (pObj, []), (pObj, []), (pObj, [])] ptrAlign


apply1 :: LlvmVar -> LlvmVar -> LlvmM LlvmVar
apply1 thunk obj1
 = do	addGlobalFuncDecl apply1FD
	result	<- newUniqueNamedReg "result" pObj
	addBlock [ Assignment result (Call StdCall (funcVarOfDecl apply1FD) [thunk, obj1] []) ]
	return result

apply2 :: LlvmVar -> LlvmVar -> LlvmVar -> LlvmM LlvmVar
apply2 thunk obj1 obj2
 = do	addGlobalFuncDecl apply2FD
	result	<- newUniqueNamedReg "result" pObj
	addBlock [ Assignment result (Call StdCall (funcVarOfDecl apply2FD) [thunk, obj1, obj2] []) ]
	return result

apply3 :: LlvmVar -> LlvmVar -> LlvmVar -> LlvmVar -> LlvmM LlvmVar
apply3 thunk obj1 obj2 obj3
 = do	addGlobalFuncDecl apply3FD
	result	<- newUniqueNamedReg "result" pObj
	addBlock [ Assignment result (Call StdCall (funcVarOfDecl apply3FD) [thunk, obj1, obj2, obj3] []) ]
	return result

apply4 :: LlvmVar -> LlvmVar -> LlvmVar -> LlvmVar -> LlvmVar -> LlvmM LlvmVar
apply4 thunk obj1 obj2 obj3 obj4
 = do	addGlobalFuncDecl apply4FD
	result	<- newUniqueNamedReg "result" pObj
	addBlock [ Assignment result (Call StdCall (funcVarOfDecl apply4FD) [thunk, obj1, obj2, obj3, obj4] []) ]
	return result


applyN :: LlvmVar -> [LlvmVar] -> LlvmM LlvmVar
applyN thunk params
 = case params of
	[a]		-> apply1 thunk a
	[a, b]		-> apply2 thunk a b
	[a, b, c]	-> apply3 thunk a b c
	[a, b, c, d]	-> apply4 thunk a b c d
	_		-> panic stage $ "applyN " ++ show (length params)

