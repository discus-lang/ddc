{-# OPTIONS -fwarn-unused-imports -fno-warn-type-defaults -cpp #-}

-- | Helpers for converting Sea to LLVM code.
module Llvm.Exp
	( llvmOfExp )
where

import DDC.Base.DataFormat
import DDC.Base.Literal
import DDC.Main.Error
import DDC.Sea.Exp
import DDC.Sea.Pretty

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

llvmOfExp (XLit (LLit (LiteralFmt (LString s) Unboxed)))
 = do	gname		<- newUniqueName "str"
	let name	= LMGlobalVar gname (typeOfString s) Internal Nothing ptrAlign True
	addGlobalVar	( name, Just (LMStaticStr s (typeOfString s)) )
	reg		<- newUniqueReg pChar
	addBlock	[ Assignment reg (GetElemPtr True (pVarLift name) [ i32LitVar 0, i32LitVar 0 ]) ]
	return		reg

llvmOfExp (XArgData (XVar (NSlot _ n) _) i)
 = do	let indx        = fst $ structFieldLookup ddcData "args"
	pobj		<- readSlot n
	pdata		<- newUniqueNamedReg "pdata" pStructData
	args		<- newUniqueReg ppObj
	ret		<- newUniqueReg pObj
	addBlock	[ Assignment pdata (Cast LM_Bitcast pobj pStructData)
			, Assignment args (GetElemPtr True pdata [ i32LitVar 0, i32LitVar indx, i32LitVar i ])
			, Assignment ret (Load args) ]
	return		ret

llvmOfExp (XVar v@NCafPtr{} tv)
 = do	r1		<- newUniqueNamedReg "r1" $ toLlvmType tv
	r2		<- newUniqueNamedReg "r2" $ toLlvmType tv
	addBlock	[ Assignment r1 (loadAddress (pVarLift (toLlvmCafVar (varOfName v) tv)))
			, Assignment r2 (loadAddress r1) ]
	return		r2

llvmOfExp (XVar v@NCaf{} tv@(TCon (TyConUnboxed _)))
 = do	r1		<- newUniqueNamedReg "r1" $ toLlvmType tv
	addBlock	[ Comment [ stage ++ " " ++ show __LINE__ ++ " " ++ show tv ]
			, Assignment r1 (loadAddress (toLlvmCafVar (varOfName v) tv)) ]
	return		r1

llvmOfExp (XLit (LLit lit))
 = 	return $ llvmVarOfLit lit

llvmOfExp (XVar n@NSuper{} tv)
 = panic stage $ "llvmOfExp (" ++ (show __LINE__) ++ ") :\n"
	++ show n ++ "\n"

llvmOfExp src
 = panic stage $ "llvmOfExp (" ++ (show __LINE__) ++ ") :\n"
	++ show src ++ "\n"

--------------------------------------------------------------------------------

llvmOfXPrim :: Prim -> [Exp a] -> LlvmM LlvmVar
llvmOfXPrim (MBox (TCon (TyConUnboxed v))) [ XLit (LLit litfmt) ]
 =	boxLit litfmt

llvmOfXPrim (MApp PAppCall) (exp@(XVar (NSuper fv) (TFun at rt)):args)
 | length at == length args
 = do	let func	= funcDeclOfExp exp
	addGlobalFuncDecl func
	params		<- mapM llvmOfExp args
	result		<- newUniqueNamedReg "result" $ toLlvmType rt
	addBlock	[ Assignment result (Call TailCall (funcVarOfDecl func) params []) ]
	return		result

 | otherwise
 =	panic stage $ "llvmOfXPrim (" ++ show __LINE__ ++ ")"
		++ "\n    types : " ++ show at
		++ "\n    args  : " ++ show args


llvmOfXPrim (MApp PAppCall) (exp@(XVar (NSuper fv) rt@(TPtr (TCon TyConObj))):[])
 = do	addComment	$ "TODO: " ++ stage ++ " " ++ show __LINE__ ++ " the type is wrong. Generatring llvm code anyway."
	let func	= funcDeclOfExp exp
	addGlobalFuncDecl func
	result		<- newUniqueNamedReg "result" $ toLlvmType rt
	addBlock	[ Assignment result (Call TailCall (funcVarOfDecl func) [] []) ]
	return		result

llvmOfXPrim (MApp PAppCall) (exp@(XVar (NSuper fv) rt@(TCon (TyConUnboxed tv))):[])
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

llvmOfXPrim (MFun PFunForce) [ XVar v@NCafPtr{} tv@(TPtr (TCon TyConObj)) ]
 = do	r1		<- newUniqueNamedReg "r1" $ toLlvmType tv
	r2		<- newUniqueNamedReg "r2" $ toLlvmType tv
	addBlock	[ Assignment r1 (loadAddress (pVarLift (toLlvmCafVar (varOfName v) tv)))
			, Assignment r2 (loadAddress r1) ]
	forceObj	r2

llvmOfXPrim (MApp PAppApply) ((fptr@(XVar n t)):args)
 = do	addComment $ "llvmOfXPrim PAppApply (" ++ (show __LINE__) ++ ")"
	params	<- mapM llvmOfExp args
	func	<- llvmOfExp fptr
	applyN	func params

llvmOfXPrim (MAlloc (PAllocThunk v t arity argc)) args
 | length args <= argc
 = do	addGlobalFuncDecl $ funcDeclOfExp (XVar (NSuper v) t)
	allocThunk (toLlvmGlobalVar v t) arity argc

llvmOfXPrim (MAlloc (PAllocData v arity)) []
 = do	tag	<- getTag $ seaVar False v
	allocData tag arity


llvmOfXPrim op args
 = panic stage $ "llvmOfXPrim (" ++ (show __LINE__) ++ ")\n\n"
	++ show op ++ "\n\n"
	++ show args ++ "\n"

--------------------------------------------------------------------------------

mkOpFunc :: PrimOp -> (LlvmVar -> LlvmVar -> LlvmExpression)
mkOpFunc op
 = case op of
	OpAdd	-> LlvmOp LM_MO_Add
	OpSub	-> LlvmOp LM_MO_Sub
	OpMul	-> LlvmOp LM_MO_Mul
	OpDiv	-> LlvmOp LM_MO_SDiv
	OpMod	-> LlvmOp LM_MO_SRem

	-- Comparison.
	OpEq	-> Compare LM_CMP_Eq
	OpNeq	-> Compare LM_CMP_Ne

	-- Use the signed versions of these for now. However, LLVM also has
	-- unsigned comparision for operating on unsigned values.
	OpGt	-> Compare LM_CMP_Sgt
	OpGe	-> Compare LM_CMP_Sge
	OpLt	-> Compare LM_CMP_Slt
	OpLe	-> Compare LM_CMP_Sle

	-- boolean
	OpAnd	-> LlvmOp LM_MO_And
	OpOr	-> LlvmOp LM_MO_Or

	-- Not a binary operatior. Needs to be handled elsewhere.
	OpNeg	-> panic stage $ "mkOpFunc (" ++ (show __LINE__) ++ ") : Unhandled op : " ++ show op


opResultType :: PrimOp -> LlvmVar -> LlvmType
opResultType op var
 = case op of
	-- comparison
	OpEq	-> i1
	OpNeq	-> i1
	OpGt	-> i1
	OpGe	-> i1
	OpLt	-> i1
	OpLe	-> i1

	-- boolean
	OpAnd	-> i1
	OpOr	-> i1

	-- normal operators like OpNeg, OpAdd, OpSub etc
	_	-> getVarType var

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

