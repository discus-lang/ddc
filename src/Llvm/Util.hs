{-# OPTIONS -fwarn-unused-imports #-}

-- | Helpers for converting Sea to LLVM code.
module Llvm.Util
	( toLlvmType
	, toLlvmVar

	, llvmWordLitVar
	, llvmIntLitVar

	, newUniqueReg
	, newUniqueLabel

	, loadAddress
	, isGlobalVar

	, pChar
	, ddcObj
	, structObj
	, pObj
	, ppObj
	, nullObj
	, ptrAlign )
where

import DDC.Base.DataFormat
import DDC.Base.Literal
import DDC.Main.Error
import DDC.Var

import qualified Config.Config		as Config

import Llvm
import Llvm.GhcReplace.Unique

import Sea.Exp
import Sea.Pretty


stage = "Llvm.Util"


-- | Convert a Sea type to an LlvmType.
toLlvmType :: Type -> LlvmType
toLlvmType (TPtr t)	= LMPointer (toLlvmType t)
toLlvmType TObj		= structObj
toLlvmType TVoid	= LMVoid
toLlvmType t		= panic stage $ "toLlvmType " ++ show t ++ "\n"


-- | Convert a Sea Var (wit a Type) to a typed LlvmVar.
toLlvmVar :: Var -> Type -> LlvmVar
toLlvmVar v t
 = case isGlobalVar v of
	True -> LMGlobalVar (seaVar False v) (toLlvmType t) External Nothing Nothing False
	False -> LMNLocalVar (seaVar True v) (toLlvmType t)


-- | Convert an Integral to an LLVM literal variable with the same size
-- as the target pointer type.
llvmWordLitVar :: Integral a => a -> LlvmVar
llvmWordLitVar n = LMLitVar (LMIntLit (toInteger n) llvmWord)

-- | Convert a DDC LiteralFmt to an LLVM literal variable.
llvmIntLitVar :: LiteralFmt -> LlvmVar
llvmIntLitVar (LiteralFmt (LInt i) (UnboxedBits 32)) = LMLitVar (LMIntLit (toInteger i) i32)
llvmIntLitVar (LiteralFmt (LInt i) (UnboxedBits 64)) = LMLitVar (LMIntLit (toInteger i) i64)


-- | Generate a new unique register variable.
newUniqueReg :: LlvmType -> IO LlvmVar
newUniqueReg t
 = do	u <- newUnique "r"
	return $ LMLocalVar u t


-- | Generate a new unique register variable.
newUniqueLabel :: String -> IO LlvmVar
newUniqueLabel label
 = do	u <- newUnique label
	return $ LMLocalVar u LMLabel


-- | Lift an LlvmVar into an expression to 'load from address of the variable'.
loadAddress :: LlvmVar -> LlvmExpression
loadAddress v = Load (pVarLift v)


-- | Does the given Sea variable have global scope? TODO: Move this to the Sea stuff.
isGlobalVar :: Var -> Bool
isGlobalVar v 
 -- If the variable is explicitly set as global use the given name.
 | bool : _	<- [global | ISeaGlobal global <- varInfo v]
 = bool

 | otherwise
 = False

--------------------------------------------------------------------------------
-- Types and variables.

pChar :: LlvmType
pChar = LMPointer i8


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

