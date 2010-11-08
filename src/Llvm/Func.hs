{-# OPTIONS -fwarn-unused-imports -fno-warn-type-defaults -cpp #-}

-- | Helpers for converting Sea to LLVM code.
module Llvm.Func
	( toLlvmFuncDecl
	, llvmFunParam )
where

import DDC.Main.Error
import DDC.Sea.Exp
import DDC.Sea.Pretty

import Llvm
import LlvmM
import Llvm.Runtime
import Llvm.Util
import Llvm.Var


stage = "Llvm.Func"

toLlvmFuncDecl :: LlvmLinkageType -> Var -> Type -> [Exp a] -> LlvmFunctionDecl
toLlvmFuncDecl linkage v t args
 = LlvmFunctionDecl {
	--  Unique identifier of the function
	decName = seaVar False v,
	--  LinkageType of the function
	funcLinkage = linkage,
	--  The calling convention of the function
	funcCc = CC_Ccc,
	--  Type of the returned value
	decReturnType = toLlvmType t,
	--  Indicates if this function uses varargs
	decVarargs = FixedArgs,
	--  Parameter types and attributes
	decParams = map toDeclParam args,
	--  Function align value, must be power of 2
	funcAlign = ptrAlign
	}



toDeclParam :: Exp a -> LlvmParameter
toDeclParam (XVar (NSlot v i) t)
 = (toLlvmType t, [])

toDeclParam x
 = panic stage $ "toDeclParam (" ++ (show __LINE__) ++ ") : " ++ show x



llvmFunParam :: Exp a -> LlvmM LlvmVar

llvmFunParam (XVar (NSlot v i) _)
 = 	readSlot i

llvmFunParam (XVar n t)
 =	return $ toLlvmVar (varOfName n) t

llvmFunParam p
 = panic stage $ "(" ++ (show __LINE__) ++ ") llvmFunParam " ++ show p


