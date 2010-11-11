{-# OPTIONS -fwarn-unused-imports -fno-warn-type-defaults -cpp #-}

-- | Helpers for converting Sea to LLVM code.
module Llvm.Func
	( toLlvmFuncDecl )
where

import DDC.Base.DataFormat
import DDC.Base.Literal
import DDC.Main.Error
import DDC.Sea.Exp
import DDC.Sea.Pretty

import Llvm
import Llvm.Util


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
toDeclParam (XVar _ t)
 = (toLlvmType t, [])

toDeclParam (XLit (LLit (LiteralFmt (LInt _) (UnboxedBits bits))))
 = case bits of
	32 -> (i32, [])
	64 -> (i64, [])
	_ -> panic stage $ "toDeclParam (" ++ show __LINE__ ++ ") " ++ show bits ++ " bits"

toDeclParam x
 = panic stage $ "toDeclParam (" ++ (show __LINE__) ++ ") :\n" ++ show x ++ "\n\n"



