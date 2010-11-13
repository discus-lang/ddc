{-# OPTIONS -fwarn-unused-imports -fno-warn-type-defaults -cpp #-}

-- | Helpers for converting Sea to LLVM code.
module Llvm.Exp
	( llvmOfExp )
where

import DDC.Base.DataFormat
import DDC.Base.Literal
import DDC.Main.Error
import DDC.Sea.Exp

import Llvm
import LlvmM
import Llvm.Func
import Llvm.Runtime
import Llvm.Util
import Llvm.Var


stage = "Llvm.Exp"

llvmOfExp :: Exp a -> LlvmM LlvmVar
llvmOfExp (XVar v2@NRts{} t2@(TPtr t@(TPtr (TCon TyConObj))))
 = do	reg		<- newUniqueReg $ toLlvmType t2
	addBlock	[ Assignment reg (loadAddress (toLlvmRtsVar (varOfName v2) t2)) ]
	return		reg

llvmOfExp (XVar (NSlot _ n) _)
 =	readSlot n

llvmOfExp (XVar n@NAuto{} t)
 = do	reg		<- newUniqueReg $ toLlvmType t
	addBlock	[ Assignment reg (loadAddress (toLlvmVar (varOfName n) t)) ]
	return		reg

llvmOfExp (XPrim op args)
 =	llvmOfXPrim op args

llvmOfExp (XLit (LLit lit))
 = 	return $ llvmVarOfLit lit


llvmOfExp (XVar n@NSuper{} tv)
 = panic stage $ "llvmOfExp (" ++ (show __LINE__) ++ ") :\n"
	++ show n ++ "\n"

llvmOfExp (XVar n@NCafPtr{} tv)
 = panic stage $ "llvmOfExp (" ++ (show __LINE__) ++ ") :\n"
	++ show n ++ "\n"

llvmOfExp (XVar n@NCaf{} tv)
 = panic stage $ "llvmOfExp (" ++ (show __LINE__) ++ ") :\n"
	++ show n ++ "\n"



llvmOfExp src
 = panic stage $ "llvmOfExp (" ++ (show __LINE__) ++ ") :\n"
	++ show src ++ "\n"

--------------------------------------------------------------------------------

llvmOfXPrim :: Prim -> [Exp a] -> LlvmM LlvmVar
llvmOfXPrim (MBox (TCon (TyConUnboxed v))) [ XLit (LLit litfmt) ]
 =	boxLit litfmt

llvmOfXPrim (MApp PAppCall) (exp@(XVar (NSuper fv) (TFun _ rt)):args)
 = do	let func	= funcDeclOfExp exp
	addGlobalFuncDecl func
	params		<- mapM llvmOfExp args
	result		<- newUniqueNamedReg "result" $ toLlvmType rt
	addBlock	[ Assignment result (Call TailCall (funcVarOfDecl func) params []) ]
	return		result

llvmOfXPrim (MApp PAppCall) (exp@(XVar (NSuper fv) rt@(TPtr (TCon TyConObj))):[])
 = do	let func	= funcDeclOfExp exp
	addGlobalFuncDecl func
	result		<- newUniqueNamedReg "result" $ toLlvmType rt
	addBlock	[ Assignment result (Call TailCall (funcVarOfDecl func) [] []) ]
	return		result

llvmOfXPrim (MOp OpAdd) [XVar v@NRts{} (TPtr t), XLit (LLit (LiteralFmt (LInt i) Unboxed)) ]
 = do	src		<- newUniqueReg $ pLift $ toLlvmType t
	next		<- newUniqueReg $ pLift $ toLlvmType t
	addBlock	[ Assignment src (loadAddress (pVarLift (toLlvmRtsVar (varOfName v) t)))
			, Assignment next (GetElemPtr True src [llvmWordLitVar i]) ]
	return		next

llvmOfXPrim (MOp op) [l, r]
 = do	lhs		<- llvmOfExp l
	rhs		<- llvmOfExp r
	result		<- newUniqueReg $ opResultType op lhs
	addBlock	[ Assignment result (mkOpFunc op lhs rhs) ]
	return		result

llvmOfXPrim (MBox t@(TCon _)) [ x ]
 =	boxExp t x

llvmOfXPrim (MUnbox t@(TCon (TyConUnboxed tt))) [ x ]
 =	unboxExp t x

llvmOfXPrim (MFun PFunForce) [ XVar (NSlot v i) (TPtr (TCon TyConObj)) ]
 = do	var <- readSlot i
	forceObj var

llvmOfXPrim (MApp PAppApply) ((fptr@(XVar n t)):args)
 = do	addComment $ "llvmOfXPrim PAppApply (" ++ (show __LINE__) ++ ")"
	params	<- mapM llvmOfExp args
	func	<- llvmOfExp fptr
	applyN	func params

llvmOfXPrim (MAlloc (PAllocThunk v t arity argc)) args
 | length args == argc
 =	allocThunk (toLlvmGlobalVar v t) arity argc


llvmOfXPrim op args
 = panic stage $ "llvmOfXPrim (" ++ (show __LINE__) ++ ")\n"
	++ show op ++ "\n"
	++ show args ++ "\n"

--------------------------------------------------------------------------------

mkOpFunc :: PrimOp -> (LlvmVar -> LlvmVar -> LlvmExpression)
mkOpFunc op
 = case op of
	OpAdd	-> LlvmOp LM_MO_Add
	OpSub	-> LlvmOp LM_MO_Sub
	OpMul	-> LlvmOp LM_MO_Mul

	OpEq	-> Compare LM_CMP_Eq
	_	-> panic stage $ "mkOpFunc (" ++ (show __LINE__) ++ ") : Unhandled op : " ++ show op


opResultType :: PrimOp -> LlvmVar -> LlvmType
opResultType op var
 = case op of
	OpAdd	-> getVarType var
	OpSub	-> getVarType var
	OpMul	-> getVarType var

	OpEq	-> i1
	_	-> panic stage $ "opResultType (" ++ (show __LINE__) ++ ") : Unhandled op : " ++ show op

--------------------------------------------------------------------------------

boxExp :: Type -> Exp a -> LlvmM LlvmVar
boxExp t (XLit lit@(LLit litfmt))
 =	boxLit litfmt

boxExp t@(TCon (TyConUnboxed tv)) var
 = do	reg		<- llvmOfExp var
	boxAny		reg

boxExp t x
 = panic stage $ "boxExp (" ++ (show __LINE__) ++ ") :\n    " ++ show t ++ "\n    " ++ (show x)

--------------------------------------------------------------------------------

unboxExp :: Type -> Exp a -> LlvmM LlvmVar
unboxExp t (XVar (NSlot _ i) (TPtr (TCon TyConObj)))
 = do	reg		<- readSlot i
	unboxAny	(toLlvmType t) reg

unboxExp t (XVar v1@NCafPtr{} tv@(TPtr (TCon TyConObj)))
 = do	r1		<- newUniqueNamedReg "r1" $ toLlvmType tv
	r2		<- newUniqueNamedReg "r2" $ toLlvmType tv
	addBlock	[ Assignment r1 (loadAddress (pVarLift (toLlvmCafVar (varOfName v1) tv)))
			, Assignment r2 (loadAddress r1) ]
	unboxAny	(toLlvmType t) r2

unboxExp t x
 = panic stage $ "unboxExp (" ++ (show __LINE__) ++ ") :\n    " ++ show t ++ "\n    " ++ (show x)

