{-# OPTIONS -fwarn-unused-imports #-}

-- | Helpers for converting Sea to LLVM code.
module Llvm.Util
	( llvmWordLitVar
	, i32LitVar
	, i64LitVar

	, newUniqueReg
	, newUniqueNamedReg
	, newUniqueLabel

	, loadAddress
	, uniqueOfLlvmVar
	, nameOfLlvmVar

	, funcOfDecl

	, pChar
	, ppChar
	, ddcObj
	, structObj
	, pObj
	, ppObj
	, nullObj
	, ptrAlign )
where

import Llvm
import Llvm.GhcReplace.Unique

import qualified Config.Config		as Config


-- stage = "Llvm.Util"


-- | Convert an Integral to an LLVM literal variable with the same size
-- as the target pointer type.
llvmWordLitVar :: Integral a => a -> LlvmVar
llvmWordLitVar n = LMLitVar (LMIntLit (toInteger n) llvmWord)

i32LitVar :: Integral a => a -> LlvmVar
i32LitVar n = LMLitVar (LMIntLit (toInteger n) i32)

i64LitVar :: Integral a => a -> LlvmVar
i64LitVar n = LMLitVar (LMIntLit (toInteger n) i64)

-- | Generate a new unique register variable with the specified LlvmType.
newUniqueReg :: LlvmType -> IO LlvmVar
newUniqueReg t
 = do	u <- newUnique "r"
	return $ LMLocalVar u t


-- | Generate a new unique named register variable with the specified LlvmType.
newUniqueNamedReg :: String -> LlvmType -> IO LlvmVar
newUniqueNamedReg name t
 = do	u <- newUnique name
	return $ LMLocalVar u t


-- | Generate a new unique register variable.
newUniqueLabel :: String -> IO LlvmVar
newUniqueLabel label
 = do	u <- newUnique label
	return $ LMLocalVar u LMLabel

uniqueOfLlvmVar :: LlvmVar -> Unique
uniqueOfLlvmVar (LMLocalVar u LMLabel) = u

nameOfLlvmVar :: LlvmVar -> String
nameOfLlvmVar (LMLocalVar u _) = show u
nameOfLlvmVar (LMNLocalVar n _) = n


-- | Lift an LlvmVar into an expression to 'load from address of the variable'.
loadAddress :: LlvmVar -> LlvmExpression
loadAddress v = Load (pVarLift v)


-- Convert a LlvmFUnctionDecl into an LlvmVar containing a function that can
-- actually be called.
funcOfDecl :: LlvmFunctionDecl -> LlvmVar
funcOfDecl decl@(LlvmFunctionDecl name _ _ _ _ _ _ )
 = LMGlobalVar name (LMFunction decl) External Nothing Nothing True

--------------------------------------------------------------------------------
-- Types and variables.

pChar :: LlvmType
pChar = LMPointer i8

ppChar :: LlvmType
ppChar = pLift pChar


ddcObj :: LlvmType
ddcObj = LMStruct [ i32 ]


structObj :: LlvmType
structObj = LMAlias ("struct.Obj", ddcObj)


pObj :: LlvmType
pObj = pLift structObj

ppObj :: LlvmType
ppObj = pLift pObj


nullObj :: LlvmVar
nullObj = LMLitVar (LMNullLit pObj)

ptrAlign :: Maybe Int
ptrAlign = Just Config.pointerBytes

