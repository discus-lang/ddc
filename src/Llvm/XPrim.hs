{-# OPTIONS -fwarn-unused-imports -fno-warn-type-defaults -cpp #-}

-- | Helpers for converting Sea to LLVM code.
module Llvm.XPrim
	( llvmOfXPrim )
where

import DDC.Base.DataFormat
import DDC.Base.Literal
import DDC.Main.Error
import DDC.Sea.Exp
import DDC.Var
import DDC.Var.PrimId

import Llvm
import LlvmM
import Llvm.Func
import Llvm.Runtime
import Llvm.Util
import Llvm.Var


stage = "Llvm.XPrim"

llvmOfXPrim :: Prim -> [Exp a] -> LlvmM LlvmVar
llvmOfXPrim (MBox (TCon (TyConUnboxed v))) [ XLit (LLit (LiteralFmt (LInt i) (UnboxedBits 32))) ]
 | varId v == VarIdPrim (TInt (UnboxedBits 32))
 =	boxInt32 $ i32LitVar i

llvmOfXPrim (MApp PAppCall) ((XVar (NSuper fv) ftype@(TFun pt rt)):args)
 | rt == TPtr (TCon TyConObj)
 = do	let func	= toLlvmFuncDecl External fv rt args
	addGlobalFuncDecl func
	params		<- mapM llvmFunParam args
	result		<- newUniqueNamedReg "result" pObj
	addBlock	[ Assignment result (Call TailCall (funcVarOfDecl func) params []) ]
	return		result

llvmOfXPrim (MApp PAppCall) ((XVar (NSuper fv) rt@(TPtr (TCon TyConObj))):[])
 = do	let func	= toLlvmFuncDecl External fv rt []
	addGlobalFuncDecl func
	result		<- newUniqueNamedReg "result" pObj
	addBlock	[ Assignment result (Call TailCall (funcVarOfDecl func) [] []) ]
	return		result


llvmOfXPrim (MOp OpAdd) [XVar v@NRts{} (TPtr t), XLit (LLit (LiteralFmt (LInt i) Unboxed)) ]
 = do	src		<- newUniqueReg $ pLift $ toLlvmType t
	next		<- newUniqueReg $ pLift $ toLlvmType t
	addBlock	[ Assignment src (loadAddress (pVarLift (toLlvmRtsVar (varOfName v) t)))
			, Assignment next (GetElemPtr True src [llvmWordLitVar i]) ]
	return		next

llvmOfXPrim (MBox t@(TCon (TyConAbstract tt))) [ x ]
 | varName tt == "String#"
 =	boxExp t x

llvmOfXPrim (MFun PFunForce) [ XVar (NSlot v i) (TPtr (TCon TyConObj)) ]
 = do	var <- readSlot i
	forceObj var


llvmOfXPrim op args
 = panic stage $ "llvmOfXPrim (" ++ (show __LINE__) ++ ")\n"
	++ show op ++ "\n"
	++ show args ++ "\n"




boxExp :: Type -> Exp a -> LlvmM LlvmVar
boxExp t (XLit lit@(LLit (LiteralFmt (LInt value) (UnboxedBits 32))))
 =	boxInt32 $ i32LitVar value


boxExp t lit@(XLit (LLit (LiteralFmt (LString s) Unboxed)))
 = do	gname	<- newUniqueName "str"
	let svar	= LMGlobalVar gname (typeOfString s) Internal Nothing ptrAlign True
	addGlobalVar	( svar, Just (LMStaticStr s (typeOfString s)) )
	boxAny		svar

boxExp t x
 = panic stage $ "(" ++ (show __LINE__) ++ ") Unhandled : boxExp\n    " ++ show t ++ "\n    " ++ (show x)

