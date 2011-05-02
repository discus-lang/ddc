{-# LANGUAGE CPP #-}
{-# OPTIONS -fwarn-unused-imports -fno-warn-type-defaults #-}

-- | Helpers for converting Sea to LLVM code.
module Llvm.Var
	( toLlvmVar
	, toLlvmGlobalVar
	, toLlvmCafVar
	, toLlvmRtsVar
	, genericFunPtrVar
	, llvmVarOfXVar
	, isGlobalVar
	, toGlobalSeaVar )
where

import DDC.Main.Error
import DDC.Base.SourcePos
import DDC.Sea.Exp
import DDC.Sea.Pretty
import DDC.Var

import Llvm
import LlvmM
import Llvm.Runtime.Object
import Llvm.Util

import Data.List					(isSuffixOf)

stage = "Llvm.Var"

-- | Convert a Sea Var (with a Type) to a typed LlvmVar.
toLlvmVar :: Var -> Type -> LlvmVar
toLlvmVar v t
 = if isGlobalVar v
	then LMGlobalVar (seaVar False v) (toLlvmType t) External Nothing (alignOfType t) False
	else LMNLocalVar (seaVar True v) (toLlvmType t)

toLlvmGlobalVar v t
 = LMGlobalVar (seaVar False v) (toLlvmType t) External Nothing (alignOfType t) False


toLlvmCafVar :: Var -> Type -> LlvmM LlvmVar
toLlvmCafVar v t@(TPtr _)
 = do	let lvar = LMGlobalVar ("_ddcCAF_" ++ seaVar False v) (toLlvmType t) External Nothing Nothing False
	mId	<- getModuleId
	if mId == varModuleId v
	  then return lvar
	  else
	    do	-- TODO : Need double lift here because the LLVM pretty printer
		-- lowers globals during code gen. This should be fixed by
		-- removing the pLower in the code gen.
		addGlobalVar (pVarLift (pVarLift lvar), Nothing)
		return lvar

toLlvmCafVar v t
 = do	let lvar = LMGlobalVar ("_ddcCAF_" ++ seaVar False v) (toLlvmType t) External Nothing Nothing False
	mId	<- getModuleId
	if mId == varModuleId v
	  then return lvar
	  else
	    do	addGlobalVar (pVarLift lvar, Nothing)
		return lvar

toGlobalSeaVar :: Var -> Type -> LlvmM LlvmVar
toGlobalSeaVar v t@(TPtr _)
 = do	let lvar	= LMGlobalVar (seaVar False v) (toLlvmType t) External Nothing Nothing False
	mId		<- getModuleId
	addGlobalVar	(pVarLift lvar, Nothing)
	return		lvar

toLlvmRtsVar :: Var -> Type -> LlvmVar
toLlvmRtsVar v t
 = LMGlobalVar (tail $ seaVar False v) (toLlvmType t) External Nothing Nothing False



llvmVarOfXVar :: Exp a -> LlvmVar
llvmVarOfXVar (XVar (NRts v) t)
 = LMGlobalVar (seaVar False v) (toLlvmType t) External Nothing (alignOfType t) False

llvmVarOfXVar exp
 = panic stage $ "llvmVarOfXVar (" ++ show __LINE__ ++ ")\n"
	++ show exp

-- | Turn the given Var (really just a name of a function) into a genereric
-- function pointer of type genericFunPtrType.
genericFunPtrVar :: Var -> LlvmVar
genericFunPtrVar v
 = LMGlobalVar (seaVar False v) genericFunPtrType External Nothing ptrAlign False



-- | Does the given Sea variable have global scope? TODO: Move this to the Sea stuff.
isGlobalVar :: Var -> Bool
isGlobalVar v
 -- If the variable is explicitly set as global use the given name.

 | bool : _	<- [global |  ISeaGlobal global
				<- concat $ [varInfo bound | IBoundBy bound <- varInfo v]]
 = bool

 | file : _	<- [sfile | ISourcePos (SourcePos (sfile, _, _))
		<-  concat [varInfo bound | IBoundBy bound <- varInfo v]]
 = ".di" `isSuffixOf` file

 | otherwise
 = False

