{-# OPTIONS -fwarn-unused-imports -fno-warn-type-defaults -cpp #-}

-- | Helpers for converting Sea to LLVM code.
module Llvm.Util
	( llvmWordLitVar
	, i32LitVar
	, i64LitVar
	, llvmVarOfLit

	, loadAddress
	, uniqueOfLlvmVar
	, nameOfLlvmVar

	, funcVarOfDecl
	, nameOfFunDecl

	, sizeOfLlvmType
	, offsetOfIndex

	, genericFunPtrType

	, toLlvmType
	, typeOfString
	, alignOfType

	, pChar
	, ppChar
	, pInt32 )
where

import DDC.Base.DataFormat
import DDC.Base.Literal
import DDC.Main.Error
import DDC.Sea.Exp
import DDC.Var

import Llvm
import Llvm.GhcReplace.Unique
import Llvm.Runtime.Object


stage = "Llvm.Util"


-- | Convert an Integral to an LLVM literal variable with the same size
-- as the target pointer type.
llvmWordLitVar :: Integral a => a -> LlvmVar
llvmWordLitVar n = LMLitVar (LMIntLit (toInteger n) llvmWord)

i32LitVar :: Integral a => a -> LlvmVar
i32LitVar n = LMLitVar (LMIntLit (toInteger n) i32)

i64LitVar :: Integral a => a -> LlvmVar
i64LitVar n = LMLitVar (LMIntLit (toInteger n) i64)


llvmVarOfLit :: LiteralFmt -> LlvmVar
llvmVarOfLit (LiteralFmt (LInt i) (UnboxedBits bits))
 = case bits of
	32	-> i32LitVar i
	64	-> i64LitVar i


uniqueOfLlvmVar :: LlvmVar -> Unique
uniqueOfLlvmVar (LMLocalVar u LMLabel) = u

nameOfLlvmVar :: LlvmVar -> String
nameOfLlvmVar (LMLocalVar u _) = show u
nameOfLlvmVar (LMNLocalVar n _) = n


-- | Lift an LlvmVar into an expression to 'load from address of the variable'.
loadAddress :: LlvmVar -> LlvmExpression
loadAddress v = Load (pVarLift v)


-- Convert a LlvmFunctionDecl into an LlvmVar containing a function that can
-- actually be called.
funcVarOfDecl :: LlvmFunctionDecl -> LlvmVar
funcVarOfDecl decl@(LlvmFunctionDecl name _ _ _ _ _ _ )
 = LMGlobalVar name (LMFunction decl) External Nothing Nothing True

nameOfFunDecl :: LlvmFunctionDecl -> String
nameOfFunDecl (LlvmFunctionDecl name _ _ _ _ _ _ )
 = name

--------------------------------------------------------------------------------
-- Types and variables.

pChar :: LlvmType
pChar = LMPointer i8

ppChar :: LlvmType
ppChar = pLift pChar


pInt32 :: LlvmType
pInt32 = LMPointer i32


-- | Convert a Sea type to an LlvmType.
toLlvmType :: Type -> LlvmType
toLlvmType (TPtr t)		= LMPointer (toLlvmType t)
toLlvmType (TCon TyConObj)	= structObj
toLlvmType TVoid		= LMVoid

toLlvmType (TCon (TyConUnboxed v))
 = case varName v of
	"Bool#"		-> i1
	"Int32#"	-> i32
	"Int64#"	-> i64
	name		-> panic stage $ "toLlvmType (" ++ (show __LINE__) ++ ") : unboxed " ++ name ++ "\n"

toLlvmType (TFun param ret)
 = LMPointer (LMFunction (
	LlvmFunctionDecl
	"dummy.function.name"
	Internal
	CC_Ccc
	(toLlvmType ret)
	FixedArgs
	(map (\t -> (toLlvmType t, [])) param)
	ptrAlign
	))


toLlvmType t
 = panic stage $ "toLlvmType (" ++ (show __LINE__) ++ ") : " ++ show t ++ "\n"


typeOfString :: String -> LlvmType
typeOfString s = LMArray (length s + 1) i8

alignOfType :: Type -> Maybe Int
alignOfType (TPtr _) = ptrAlign
alignOfType _ = Nothing

