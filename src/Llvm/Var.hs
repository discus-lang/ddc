{-# OPTIONS -fwarn-unused-imports -fno-warn-type-defaults -cpp #-}

-- | Helpers for converting Sea to LLVM code.
module Llvm.Var
	( toLlvmVar
	, toLlvmGlobalVar
	, toLlvmCafVar
	, toLlvmRtsVar
	, genericFunPtrVar
	, llvmVarOfXVar
	, isGlobalVar )
where

import DDC.Main.Error
import DDC.Base.SourcePos
import DDC.Sea.Exp
import DDC.Sea.Pretty
import DDC.Var

import Llvm
import Llvm.Util

import Data.List					(isSuffixOf)

stage = "Llvm.Var"

-- | Convert a Sea Var (with a Type) to a typed LlvmVar.
toLlvmVar :: Var -> Type -> LlvmVar
toLlvmVar v t
 = case isGlobalVar v of
	True -> LMGlobalVar (seaVar False v) (toLlvmType t) External Nothing (alignOfType t) False
	False -> LMNLocalVar (seaVar True v) (toLlvmType t)

toLlvmGlobalVar v t
 = LMGlobalVar (seaVar False v) (toLlvmType t) External Nothing (alignOfType t) False


toLlvmCafVar :: Var -> Type -> LlvmVar
toLlvmCafVar v t
 = LMGlobalVar ("_ddcCAF_" ++ seaVar False v) (toLlvmType t) External Nothing Nothing False


toLlvmRtsVar :: Var -> Type -> LlvmVar
toLlvmRtsVar v t
 = LMGlobalVar (tail $ seaVar False v) (toLlvmType t) External Nothing Nothing False



llvmVarOfXVar :: Exp a -> LlvmVar
llvmVarOfXVar (XVar (NRts v) t)
 = LMGlobalVar (seaVar False v) (toLlvmType t) External Nothing (alignOfType t) False

llvmVarOfXVar exp
 = panic stage $ "llvmVarOfXVar (" ++ (show __LINE__) ++ ")\n"
	++ show exp

-- | Turn the given Var (really just a name of a function) into a genereric
-- function pointer if type. See also genericFunPtrType.
genericFunPtrVar :: Var -> LlvmVar
genericFunPtrVar v
 = LMGlobalVar (seaVar False v) genericFunPtrType External Nothing ptrAlign False



-- | Does the given Sea variable have global scope? TODO: Move this to the Sea stuff.
isGlobalVar :: Var -> Bool
isGlobalVar v
 -- If the variable is explicitly set as global use the given name.
 | bool : _	<- [global | ISeaGlobal global <- varInfo v]
 = bool

 | file : _	<- [sfile | ISourcePos (SourcePos (sfile, _, _))
		<-  concat [varInfo bound | IBoundBy bound <- varInfo v]]
 = isSuffixOf ".di" file

 | otherwise
 = False

