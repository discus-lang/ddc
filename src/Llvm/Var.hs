{-# OPTIONS -fwarn-unused-imports -fno-warn-type-defaults -cpp #-}

-- | Helpers for converting Sea to LLVM code.
module Llvm.Var
	( toLlvmVar
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
toLlvmVar v t@(TFun r TVoid)
 = LMNLocalVar (seaVar True v) (toLlvmType t)

toLlvmVar v t@(TFun _ _ )
 = panic stage $ "toLlvmVar (" ++ (show __LINE__) ++ ") type : " ++ show t

toLlvmVar v t
 = case isGlobalVar v of
	True -> LMGlobalVar (seaVar False v) (toLlvmType t) External Nothing (alignOfType t) False
	False -> LMNLocalVar (seaVar True v) (toLlvmType t)

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

