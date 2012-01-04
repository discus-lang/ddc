{-# LANGUAGE CPP #-}
{-# OPTIONS -fwarn-unused-imports -fno-warn-type-defaults #-}
module Llvm.Func
	( funcDeclOfExp
	, funcDeclOfExtern )
where

import DDC.Main.Error
import DDC.Sea.Exp
import DDC.Sea.Pretty
import DDC.Var

import Llvm
import LlvmM
import Llvm.Runtime.Object
import Llvm.Util

import Control.Monad
import qualified Data.Map		as Map


stage = "Llvm.Stage"

mkFuncDecl :: Exp a -> LlvmFunctionDecl
mkFuncDecl (XVar (NSuper v) t@(TFun at rt))
 = let	(varArgs, params) = specialCaseFuncs (seaVar False v) at
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

mkFuncDecl (XVar v t)
 = panic stage $ "mkFuncDecl (" ++ show __LINE__ ++ ")\n\n"
	++ show v ++ "\n\n"
	++ show t ++ "\n"


funcDeclOfExp :: Exp a -> LlvmM LlvmFunctionDecl
funcDeclOfExp exp@(XVar (NSuper v) _)
 = do	let fdecl = mkFuncDecl exp
	modId <- getModuleId
	-- Only add a global function declaration if the function is external to
	-- the current module.
	when (varModuleId v /= modId) $
		addGlobalFuncDecl fdecl
	return fdecl


funcDeclOfExtern :: Top a -> LlvmFunctionDecl
funcDeclOfExtern pp@(PExtern v t@(TFun at rt))
 = let	(varArgs, params) = specialCaseFuncs (seaVar False v) at
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


-- Calling the C function 'printf' with just a single string argument via the
-- FFI was segfaulting (see bug #211) because LLVM didn't know that printf had
-- a variable argument list. For some reason this was not a problem as long as
-- printf was called with at least 1 extra argument.
--
-- All variable argument functions called via the FFI should be listed here.

varArgMap = Map.fromList
	[ ("printf"	, [ (pChar, []) ])
	, ("snprintf"	, [ (pChar, []), (i32, []), (pChar, []) ])
	]

specialCaseFuncs vname [TVoid]
 = case Map.lookup vname varArgMap of
	Nothing -> (FixedArgs, [])
	Just al	-> (VarArgs, al)

specialCaseFuncs vname at
 = case Map.lookup vname varArgMap of
	Nothing -> (FixedArgs, map (\t -> (toLlvmType t, [])) at)
	Just al	-> (VarArgs, al)
