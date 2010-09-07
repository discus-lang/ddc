{-# OPTIONS -fwarn-unused-imports #-}

-- | Helpers for converting Sea to LLVM code.
module Llvm.Util
	( llvmWordLitVar
	, i32LitVar
	, i64LitVar

	, loadAddress
	, uniqueOfLlvmVar
	, nameOfLlvmVar

	, funcVarOfDecl
	, nameOfFunDecl

	, sizeOfLlvmType
	, offsetOfIndex

	, ddcObj
	, ddcThunk

	, pFunction

	, pObj
	, structObj
	, pThunk
	, structThunk

	, pChar
	, ppChar
	, pInt32
	, ppObj
	, nullObj
	, ptrAlign )
where

import DDC.Main.Error

import Llvm
import Llvm.GhcReplace.Unique

import qualified Config.Config		as Config


stage = "Llvm.Util"


-- | Convert an Integral to an LLVM literal variable with the same size
-- as the target pointer type.
llvmWordLitVar :: Integral a => a -> LlvmVar
llvmWordLitVar n = LMLitVar (LMIntLit (toInteger n) llvmWord)

i32LitVar :: Integral a => a -> LlvmVar
i32LitVar n = LMLitVar (LMIntLit (toInteger n) i32)

i64LitVar :: Integral a => a -> LlvmVar
i64LitVar n = LMLitVar (LMIntLit (toInteger n) i64)




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


pFunction :: LlvmType
pFunction = LMPointer (LMFunction (LlvmFunctionDecl "dummy.function.name" Internal CC_Ccc LMVoid FixedArgs [] ptrAlign))


thunk32 :: LlvmType
thunk32
 = LMStruct 	[ i32			-- tag
		, LMPointer i8		-- function pointer
		, i32			-- arity
		, i32			-- args
		, LMArray 0 pObj	-- Pointer to arguments
		]

thunk64 :: LlvmType
thunk64
 = LMStruct 	[ LMUnion
			[ i32		-- tag
			, i64 ]		-- padding
		, pFunction 		-- function pointer
		, i32			-- arity
		, i32			-- args
		, LMArray 0 pObj	-- Pointer to arguments
		]

ddcThunk :: LlvmType
ddcThunk
 = case Config.pointerBytes of
	4 -> thunk32
	8 -> thunk64
	_ -> panic stage $ "Config.pointerBytes == " ++ show Config.pointerBytes

structThunk :: LlvmType
structThunk = LMAlias ("struct.Thunk", ddcThunk)

pThunk :: LlvmType
pThunk = pLift structThunk

sizeOfLlvmType :: LlvmType -> Int
sizeOfLlvmType t
 = case t of
	LMInt bits	-> div bits 8
	LMFloat		-> 4
	LMDouble	-> 8
	LMFloat80	-> 10
	LMFloat128	-> 16
	LMPointer _	-> Config.pointerBytes
	LMArray n t	-> n * sizeOfLlvmType t
	LMLabel		-> panic stage $ "sizeOfLlvmType LMLabel"
	LMVoid		-> panic stage $ "sizeOfLlvmType LMVoid"
	LMStruct t	-> sum $ map sizeOfLlvmType t
	LMUnion t	-> maximum $ map sizeOfLlvmType t
	LMAlias (_, t)	-> sizeOfLlvmType t

offsetOfIndex :: LlvmType -> Int -> Int
offsetOfIndex _ 0 = 0
offsetOfIndex typ i
 | i > 0
 = case typ of
	LMLabel		-> panic stage $ "offsetOfIndex LMLabel"
	LMVoid		-> panic stage $ "offsetOfIndex LMVoid"
	LMStruct t	-> sum $ take i $ map sizeOfLlvmType t
	LMAlias (_, t)	-> offsetOfIndex t i
	_		-> 0


ptrAlign :: Maybe Int
ptrAlign = Just Config.pointerBytes

