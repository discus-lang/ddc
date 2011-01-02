{-# LANGUAGE CPP #-}
{-# OPTIONS -fwarn-unused-imports -fno-warn-type-defaults #-}
module Llvm.Func
	( funcDeclOfExp )
where

import DDC.Main.Error
import DDC.Sea.Exp
import DDC.Sea.Pretty
import DDC.Var

import Llvm
import Llvm.Runtime.Object
import Llvm.Util

import qualified Data.Map		as Map


stage = "Llvm.Stage"

funcDeclOfExp :: Exp a -> LlvmFunctionDecl
funcDeclOfExp (XVar (NSuper v) t@(TFun at rt))
 = let	(varArgs, params) = specialCaseFuncs (varName v) at
   in	LlvmFunctionDecl {
		--  Unique identifier of the function
		decName = seaVar False v,
		--  LinkageType of the function
		funcLinkage = External,
		--  The calling convention of the function
		funcCc = CC_Ccc,
		--  Type of the returned value
		decReturnType = toLlvmType rt,
		--  Indicates if this function uses varargs
		decVarargs = varArgs,
		--  Parameter types and attributes
		decParams = params,
		--  Function align value, must be power of 2
		funcAlign = ptrAlign
		}

funcDeclOfExp (XVar v t)
 = panic stage $ "funcDeclOfExp (" ++ show __LINE__ ++ ")\n\n"
	++ show v ++ "\n\n"
	++ show t ++ "\n"


-- Calling the C function 'printf' with just a single string argument via the
-- FFI was segfaulting (see bug #211) because LLVM didn't know that printf had
-- a variable argument list. For some reason this was not a problem as long as
-- printf was called with at least 1 extra argument.
--
-- All variable argument functions called via the FFI should be listed here.

varArgMap = Map.fromList
	[ ("printf"	, [ (pChar, []) ])
	, ("snprintf"	, [ (pChar, []), (llvmWord, []), (pChar, []) ])
	]

specialCaseFuncs vname at
 = case Map.lookup vname varArgMap of
	Nothing -> (FixedArgs, map (\t -> (toLlvmType t, [])) at)
	Just al	-> (VarArgs, al)
