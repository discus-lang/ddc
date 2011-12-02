{-# LANGUAGE CPP #-}
{-# OPTIONS -fwarn-unused-imports -fno-warn-type-defaults #-}

-- | Helpers for converting Sea to LLVM code.
module Llvm.Exp
	( llvmOfExp )
where

import DDC.Base.DataFormat
import DDC.Base.Literal
import DDC.Base.Prim.PrimCast
import DDC.Base.Prim.PrimCoerce
import DDC.Base.Prim.PrimPtr
import DDC.Base.Prim.PrimType
import DDC.Main.Error
import DDC.Sea.Exp
import DDC.Sea.Pretty
import DDC.Var
import DDC.Var.PrimId

import Llvm
import LlvmM
import Llvm.Func
import Llvm.Runtime
import Llvm.Util
import Llvm.Var

import qualified Data.Map		as Map


stage = "Llvm.Exp"

llvmOfExp :: Exp a -> LlvmM LlvmVar
llvmOfExp (XPrim op args)
 =	llvmOfXPrim op args

llvmOfExp (XVar v2@NRts{} t2@(TPtr t@(TPtr (TCon TyConObj))))
 = do	reg		<- newUniqueReg $ toLlvmType t2
	addBlock	[ Assignment reg (loadAddress (toLlvmRtsVar (varOfName v2) t2)) ]
	return		reg

llvmOfExp (XVar (NSlot _ n) _)
 =	readSlot n

llvmOfExp (XVar n@(NAuto var) t@(TPtr (TCon (TyConUnboxed tv))))
 | varName tv == "File#"
 && seaVar False var `elem` [ "stdin", "stdout", "stderr" ]
 = do	-- Need to special case handling of C stdin/stdout/stderr.
	reg		<- newUniqueReg $ toLlvmType t
	cv		<- toGlobalSeaVar (varOfName n) t
	addBlock	[ Assignment reg (loadAddress cv) ]
	return		reg

llvmOfExp (XVar n@(NCaf var) t@(TPtr (TCon (TyConUnboxed tv))))
 | varName tv == "File#"
 && seaVar False var `elem` [ "stdin", "stdout", "stderr" ]
 = do	-- Need to special case handling of C stdin/stdout/stderr.
	reg		<- newUniqueReg $ toLlvmType t
	cv		<- toGlobalSeaVar (varOfName n) t
	addBlock	[ Assignment reg (loadAddress cv) ]
	return		reg

llvmOfExp (XVar n@NAuto{} t)
 = do	reg		<- newUniqueReg $ toLlvmType t
	addBlock	[ Assignment reg (loadAddress (toLlvmVar (varOfName n) t)) ]
	return		reg

llvmOfExp (XLit (LLit (LiteralFmt (LString s) Unboxed)))
 = do	name		<- addString s
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


llvmOfExp (XArgDataM struct (XVar (NSlot _ n) t) i)
 = do	let name	= ctorName struct
	ctorStruct	<- getCtorStruct name
	(index, ftype)	<- getCtorFieldByIndex name i
	addComment	$ "llvmOfExp XArgDataM : " ++ name ++ " " ++ show i ++ " -> " ++ show index

	let pli		= fst $ structFieldLookup ddcDataM "payload"
	pobj		<- readSlot n
	pdata		<- newUniqueNamedReg "pdata" pStructDataM
	payload		<- newUniqueNamedReg "payload" pChar
	pctor		<- newUniqueNamedReg "ctor" $ pLift $ ctorStruct
	pfield		<- newUniqueNamedReg "pval" $ pLift ftype
	ret		<- newUniqueNamedReg "ret" ftype
	addBlock	[ Assignment pdata (Cast LM_Bitcast pobj pStructDataM)
			, Assignment payload (GetElemPtr True pdata [ i32LitVar 0, i32LitVar pli, i32LitVar 0 ])
			, Assignment pctor (Cast LM_Bitcast payload (pLift ctorStruct))
			, Assignment pfield (GetElemPtr True pctor [ i32LitVar 0, i32LitVar index ])
			, Assignment ret (Load pfield) ]
	return		ret


llvmOfExp (XVar v@NCafPtr{} tv)
 = do	r1		<- newUniqueNamedReg "r1" $ toLlvmType tv
	r2		<- newUniqueNamedReg "r2" $ toLlvmType tv
	cv		<- toLlvmCafVar (varOfName v) tv
	addBlock	[ Assignment r1 (loadAddress (pVarLift cv))
			, Assignment r2 (loadAddress r1) ]
	return		r2

llvmOfExp (XVar v@NCaf{} tv@(TCon (TyConUnboxed _)))
 = do	r1		<- newUniqueNamedReg "r1" $ toLlvmType tv
	cv		<- toLlvmCafVar (varOfName v) tv
	addBlock	[ Assignment r1 (loadAddress cv) ]
	return		r1

llvmOfExp (XLit (LLit lit))
 = 	return $ llvmVarOfLit lit

llvmOfExp (XVar n@NSuper{} tv)
 = panic stage $ "\nllvmOfExp (" ++ show __LINE__ ++ ") :\n\n"
	++ show n ++ "\n\n"
	++ show tv ++ "\n\n"

llvmOfExp (XVar n@NCaf{} tv)
 = panic stage $ "\nllvmOfExp (" ++ show __LINE__ ++ ") :\n\n"
	++ show n ++ "\n\n"
	++ show tv ++ "\n\n"

llvmOfExp src
 = panic stage $ "llvmOfExp (" ++ show __LINE__ ++ ") :\n"
	++ show src ++ "\n"

--------------------------------------------------------------------------------

llvmOfXPrim :: Prim -> [Exp a] -> LlvmM LlvmVar
llvmOfXPrim (MBox (TCon (TyConUnboxed v))) [ XLit (LLit litfmt) ]
 =	boxLit litfmt

llvmOfXPrim (MApp PAppCall) exp@(XVar (NSuper fv) (TFun at rt):args)
 = case varId fv of
	VarIdPrim prim	-> primCall prim exp
	_		-> primOrFunCall exp

llvmOfXPrim (MApp PAppCall) (exp@(XVar (NSuper fv) ft):_)
 =	-- The type ft should be 'TFun _ _'. This is a bug in an earlier stage of the compiler.
	panic stage	$ "Bad type for NSuper:\n"
			++ "NSuper : " ++ show fv ++ "\n"
			++ "Type   : " ++ show ft ++ "\n\n"

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
	addBlock	[ Assignment result (mkOpFunc (getVarType lhs) op lhs rhs) ]
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
	cv		<- toLlvmCafVar (varOfName v) tv
	addBlock	[ Assignment r1 (loadAddress (pVarLift cv))
			, Assignment r2 (loadAddress r1) ]
	forceObj	r2

llvmOfXPrim (MApp PAppApply) ((fptr@(XVar n t)):args)
 = do	params	<- mapM llvmOfExp args
	func	<- llvmOfExp fptr
	applyN	func params

llvmOfXPrim (MAlloc (PAllocThunk v t arity argc)) args
 | length args <= argc
 = do	addGlobalFuncDecl $ funcDeclOfExp (XVar (NSuper v) t)
	allocThunk (toLlvmGlobalVar v t) arity argc

llvmOfXPrim (MAlloc (PAllocData v arity)) []
 = do	tag	<- getTag $ seaVar False v
	allocData tag arity

llvmOfXPrim (MAlloc (PAllocDataM v boxedCount payloadSize)) []
 = do	tag	<- getTag $ seaVar False v
	allocDataM tag boxedCount payloadSize

llvmOfXPrim (MOp OpNeg) [arg]
 = do	exp		<- llvmOfExp arg
	let typ		= getVarType exp
	r0		<- newUniqueReg typ
	addBlock	[ Assignment r0 (mkOpFunc typ OpSub (zeroLiteral typ) exp) ]
	return		r0

llvmOfXPrim (MOp OpIsZero) [ arg@(XVar n (TCon (TyConUnboxed t))) ]
 | varName t == "Addr#"
 = do	exp		<- llvmOfExp arg
	rint		<- newUniqueReg llvmWord
	bool		<- newUniqueReg i1
	addBlock	[ Assignment rint (Cast LM_Ptrtoint exp llvmWord)
			, Assignment bool (Compare LM_CMP_Eq rint (llvmWordLitVar 0))
			]
	return		bool

llvmOfXPrim (MCast pcast) [arg]
 = do	exp		<- llvmOfExp arg
	let (castop, typ)
			= intCastOp pcast
	r0		<- newUniqueReg typ
	addBlock	[ Assignment r0 (Cast castop exp typ) ]
	return		r0

llvmOfXPrim (MCoerce (PrimCoercePtr _ tdest)) [ arg ]
 = do	let destType	= pLift $ toLlvmType tdest
	exp		<- llvmOfExp arg
	r0		<- newUniqueReg destType
	addBlock	[ Assignment r0 (Cast LM_Bitcast exp destType) ]
	return		r0

llvmOfXPrim (MCoerce (PrimCoerceAddrToPtr td@(TCon _))) [ arg ]
 = do	let destType	= pLift $ toLlvmType td
	ptr		<- llvmOfExp arg
	r0		<- newUniqueReg $ destType
	addBlock	[ Assignment r0 (Cast LM_Bitcast ptr destType ) ]
	return		r0

llvmOfXPrim (MPtr PrimPtrPlus) [ arg1@(XVar _ t1@(TPtr _)), arg2 ]
 = do	ptr		<- llvmOfExp arg1
	int		<- llvmOfExp arg2
	r0		<- newUniqueReg $ toLlvmType t1
	addBlock	[ Assignment r0 (GetElemPtr True ptr [int]) ]
	return		r0


llvmOfXPrim (MPtr (PrimPtrPeek (PrimTypeWord (Width w)))) [ arg@XVar{} ]
 = do	ptr		<- llvmOfExp arg
	r0		<- newUniqueReg $ LMInt w
	addBlock	[ Assignment r0 (Load ptr) ]
	return		r0

llvmOfXPrim (MPtr (PrimPtrPeek (PrimTypeInt (Width w)))) [ arg@XVar{} ]
 = do	ptr		<- llvmOfExp arg
	r0		<- newUniqueReg $ LMInt w
	addBlock	[ Assignment r0 (Load ptr) ]
	return		r0

llvmOfXPrim (MPtr (PrimPtrPeek (PrimTypeFloat (Width 32)))) [ arg@XVar{} ]
 = do	ptr		<- llvmOfExp arg
	r0		<- newUniqueReg $ LMFloat
	addBlock	[ Assignment r0 (Load ptr) ]
	return		r0

llvmOfXPrim (MPtr (PrimPtrPeek (PrimTypeFloat (Width 64)))) [ arg@XVar{} ]
 = do	ptr		<- llvmOfExp arg
	r0		<- newUniqueReg $ LMDouble
	addBlock	[ Assignment r0 (Load ptr) ]
	return		r0

llvmOfXPrim (MPtr (PrimPtrPeekOn (PrimTypeWord (Width w)))) [ _, arg2@XVar{} ]
 = do	ptr		<- llvmOfExp arg2
	r0		<- newUniqueReg $ LMInt w
	addBlock	[ Assignment r0 (Load ptr) ]
	return		r0

llvmOfXPrim (MPtr (PrimPtrPeekOn (PrimTypeInt (Width w)))) [ _, arg2@XVar{} ]
 = do	ptr		<- llvmOfExp arg2
	r0		<- newUniqueReg $ LMInt w
	addBlock	[ Assignment r0 (Load ptr) ]
	return		r0

llvmOfXPrim (MPtr (PrimPtrPeekOn (PrimTypeFloat (Width 32)))) [ _, arg2@XVar{} ]
 = do	ptr		<- llvmOfExp arg2
	r0		<- newUniqueReg $ LMFloat
	addBlock	[ Assignment r0 (Load ptr) ]
	return		r0

llvmOfXPrim (MPtr (PrimPtrPeekOn (PrimTypeFloat (Width 64)))) [ _, arg2@XVar{} ]
 = do	ptr		<- llvmOfExp arg2
	r0		<- newUniqueReg $ LMDouble
	addBlock	[ Assignment r0 (Load ptr) ]
	return		r0


llvmOfXPrim (MPtr (PrimPtrPoke (PrimTypeInt _))) [ a1@XVar{}, a2 ]
 = do	ptr		<- llvmOfExp a1
	val		<- llvmOfExp a2
	addBlock	[ Store val ptr ]
	return		val

llvmOfXPrim (MPtr (PrimPtrPoke (PrimTypeWord _))) [ a1@XVar{}, a2 ]
 = do	ptr		<- llvmOfExp a1
	val		<- llvmOfExp a2
	addBlock	[ Store val ptr ]
	return		val

llvmOfXPrim (MPtr (PrimPtrPoke (PrimTypeFloat _))) [ a1@XVar{}, a2 ]
 = do	ptr		<- llvmOfExp a1
	val		<- llvmOfExp a2
	addBlock	[ Store val ptr ]
	return		val

llvmOfXPrim (MPtr (PrimPtrPokeOn (PrimTypeInt _))) [ _, a1@(XVar _ (TPtr _)), a2@XVar{} ]
 = do	ptr		<- llvmOfExp a1
	val		<- llvmOfExp a2
	addBlock	[ Store val ptr ]
	return		val

llvmOfXPrim (MPtr (PrimPtrPokeOn (PrimTypeWord _))) [ _, a1@(XVar _ (TPtr _)), a2@XVar{} ]
 = do	ptr		<- llvmOfExp a1
	val		<- llvmOfExp a2
	addBlock	[ Store val ptr ]
	return		val

llvmOfXPrim (MPtr (PrimPtrPokeOn (PrimTypeFloat _))) [ _, a1@(XVar _ (TPtr _)), a2@XVar{} ]
 = do	ptr		<- llvmOfExp a1
	val		<- llvmOfExp a2
	addBlock	[ Store val ptr ]
	return		val




llvmOfXPrim op args
 = panic stage $ "llvmOfXPrim (" ++ show __LINE__ ++ ")\n\n"
	++ show op ++ "\n\n"
	++ "argCount " ++ show (length args) ++ "\n\n"
	++ show args ++ "\n"

--------------------------------------------------------------------------------

intCastOp :: PrimCast -> (LlvmCastOp, LlvmType)
intCastOp (PrimCast (PrimTypeWord (Width srcw)) (PrimTypeWord (Width destw)))
 | srcw < destw
 = (LM_Zext, LMInt destw)

 | otherwise
 = (LM_Trunc, LMInt destw)

intCastOp (PrimCast (PrimTypeInt (Width srcw)) (PrimTypeInt (Width destw)))
 | srcw < destw
 = (LM_Sext, LMInt destw)

 | otherwise
 = (LM_Trunc, LMInt destw)

intCastOp (PrimCast (PrimTypeFloat (Width 32)) (PrimTypeFloat (Width 64)))
 = (LM_Fpext, LMDouble)

intCastOp (PrimCast (PrimTypeFloat (Width 64)) (PrimTypeFloat (Width 32)))
 = (LM_Fptrunc, LMFloat)

intCastOp pc
 = panic stage $ "intCastOp (" ++ show __LINE__ ++ ") " ++ show pc

--------------------------------------------------------------------------------

primOrFunCall :: [Exp a] -> LlvmM LlvmVar
primOrFunCall all@((XVar (NSuper fv) (TFun at rt)):args)
 | length at == length args || (at == [TVoid] && null args)
 = case primLookup fv of
	Nothing -> funCall all
	Just fn	-> fn args

primOrFunCall ((XVar (NSuper fv) t):args)
 = panic stage $
	"\n   Function : " ++ seaVar False fv ++
	"\n   Type     : " ++ show t ++
	"\n   arglen   : " ++ show (length args) ++ "\n"


funCall :: [Exp a] -> LlvmM LlvmVar
funCall (exp@(XVar (NSuper fv) (TFun at TVoid)):args)
 | length at == length args || (at == [TVoid] && null args)
 = do	let func	= funcDeclOfExp exp
	addGlobalFuncDecl func
	params		<- mapM llvmOfExp args
	dummy		<- newUniqueNamedReg "dummy_do_not_use" $ toLlvmType TVoid
	addBlock	[ Expr (Call TailCall (funcVarOfDecl func) params []) ]
	return		dummy

funCall (exp@(XVar (NSuper fv) (TFun at rt)):args)
 | length at == length args || (at == [TVoid] && null args)
 = do	let func	= funcDeclOfExp exp
	addGlobalFuncDecl func
	params		<- mapM llvmOfExp args
	result		<- newUniqueNamedReg "result" $ toLlvmType rt
	addBlock	[ Assignment result (Call TailCall (funcVarOfDecl func) params []) ]
	return		result

 | otherwise
 = panic stage $ "funCall (" ++ show __LINE__ ++ ")"
		++ "\n    fv   : " ++ show fv
		++ "\n    rt   : " ++ show rt
		++ "\n    at   : " ++ show at
		++ "\n    argc : " ++ show (length args)
		++ "\n    argv : " ++ show args

primCall :: PrimId -> [Exp a] -> LlvmM LlvmVar
primCall primId exp@(XVar (NSuper fv) (TFun at rt):args)
 | length at == length args
 = case primId of
	VProjFieldR	-> primProjFieldR args
	_		-> funCall exp
	-- VUnit
	-- VBoxString

 | otherwise
 = panic stage $ "primCall (" ++ show __LINE__ ++ ")"
		++ "\n    fv   : " ++ show fv
		++ "\n    rt   : " ++ show rt
		++ "\n    at   : " ++ show at
		++ "\n    argc : " ++ show (length args)
		++ "\n    argv : " ++ show args

--------------------------------------------------------------------------------

mkOpFunc :: LlvmType -> PrimOp -> LlvmVar -> LlvmVar -> LlvmExpression
mkOpFunc varType op
 = case (varType, op) of
	-- Integer addition, subtraction and multiplication..
	(LMInt _,	OpAdd)	-> LlvmOp LM_MO_Add
	(LMInt _,	OpSub)	-> LlvmOp LM_MO_Sub
	(LMInt _,	OpMul)	-> LlvmOp LM_MO_Mul

	-- Float addition, subtraction and multiplication..
	(_	,	OpAdd)	-> LlvmOp LM_MO_FAdd
	(_	,	OpSub)	-> LlvmOp LM_MO_FSub
	(_	,	OpMul)	-> LlvmOp LM_MO_FMul

	-- Integer division and remainder.
	(LMInt _,	OpDiv)	-> LlvmOp LM_MO_SDiv
	(LMInt _,	OpMod)	-> LlvmOp LM_MO_SRem

	-- Float division and remainder.
	(_	,	OpDiv)	-> LlvmOp LM_MO_FDiv
	(_	,	OpMod)	-> LlvmOp LM_MO_FRem

	-- Integer comparison.
	(LMInt _,	OpEq)	-> Compare LM_CMP_Eq
	(LMInt _,	OpNeq)	-> Compare LM_CMP_Ne

	-- Use the signed versions of these for now. However, LLVM also has
	-- unsigned comparision for operating on unsigned values.
	(LMInt _, 	OpGt)	-> Compare LM_CMP_Sgt
	(LMInt _, 	OpGe)	-> Compare LM_CMP_Sge
	(LMInt _, 	OpLt)	-> Compare LM_CMP_Slt
	(LMInt _, 	OpLe)	-> Compare LM_CMP_Sle

	-- Float comparison.
	(_	,	OpEq)	-> Compare LM_CMP_Feq
	(_	,	OpNeq)	-> Compare LM_CMP_Fne

	(_	,	OpGt)	-> Compare LM_CMP_Fgt
	(_	, 	OpGe)	-> Compare LM_CMP_Fge
	(_	, 	OpLt)	-> Compare LM_CMP_Flt
	(_	, 	OpLe)	-> Compare LM_CMP_Fle

	-- boolean
	(_	,	OpAnd)	-> LlvmOp LM_MO_And
	(_	,	OpOr)	-> LlvmOp LM_MO_Or

	-- Not a binary operatior. Needs to be handled elsewhere.
	(_	,	OpNeg)	-> panic stage $ "mkOpFunc (" ++ show __LINE__ ++ ") : Unhandled op : " ++ show op


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
 = panic stage $ "boxExp (" ++ show __LINE__ ++ ") :\n    " ++ show t ++ "\n    " ++ show x

--------------------------------------------------------------------------------

unboxExp :: Type -> Exp a -> LlvmM LlvmVar
unboxExp t (XVar (NSlot _ i) (TPtr (TCon TyConObj)))
 = do	reg		<- readSlot i
	unboxAny	(toLlvmType t) reg

unboxExp t (XVar v1@NCafPtr{} tv@(TPtr (TCon TyConObj)))
 = do	r1		<- newUniqueNamedReg "r1" $ toLlvmType tv
	r2		<- newUniqueNamedReg "r2" $ toLlvmType tv
	cv		<- toLlvmCafVar (varOfName v1) tv
	addBlock	[ Assignment r1 (loadAddress (pVarLift cv))
			, Assignment r2 (loadAddress r1) ]
	unboxAny	(toLlvmType t) r2

unboxExp t x
 = panic stage $ "unboxExp (" ++ show __LINE__ ++ ") :\n    " ++ show t ++ "\n    " ++ show x

--------------------------------------------------------------------------------

zeroLiteral :: LlvmType -> LlvmVar
zeroLiteral typ
 = case typ of
	LMInt 32	-> i32LitVar 0
	LMInt 64	-> i64LitVar 0
	LMFloat		-> LMLitVar (LMFloatLit 0.0 LMFloat)
	LMDouble	-> LMLitVar (LMFloatLit 0.0 LMDouble)

--------------------------------------------------------------------------------

primProjFieldR :: [Exp a] -> LlvmM LlvmVar
primProjFieldR args@[exp@(XVar v (TPtr (TCon TyConObj))), XLit (LLit (LiteralFmt (LInt index) (UnboxedBits 32)))]
 = do	addGlobalFuncDecl boxRef

	let boxFun	= LMGlobalVar "_boxRef" (LMFunction boxRef) External Nothing Nothing True
	let field	= fst $ structFieldLookup ddcData "args"

	pdata		<- newUniqueNamedReg "pdata" pStructData
	pi		<- newUniqueReg ppObj
	pc		<- newUniqueReg pChar
	bref		<- newUniqueReg pObj
	result		<- newUniqueReg pObj

	pobj		<- llvmOfExp exp

	addBlock	[ Assignment pdata (Cast LM_Bitcast pobj pStructData)
			, Assignment pi (GetElemPtr True pdata [i32LitVar 0, i32LitVar field, i32LitVar index])
			, Assignment pc (Cast LM_Bitcast pi pChar)
			, Assignment bref (Call StdCall boxFun [pobj, pc] [])
			, Assignment result (Cast LM_Bitcast bref pObj) ]
	return		result

--------------------------------------------------------------------------------
-- Some primitive functions have to be implemented directly in LLVM code.
--
-- The primFunMap is a map from the primitive's Sea name to a function that
-- generates LLVM code for the primitive.
--
-- TODO: There must be a nicer way to do this.

primLookup :: Var -> Maybe ([Exp a] -> LlvmM LlvmVar)
primLookup funvar
 = Map.lookup (seaVar False funvar) primFunMap


primFunMap :: Map.Map String ([Exp a] -> LlvmM LlvmVar)
primFunMap
 = Map.fromList
	[ ( "_allocDataR",			primAllocDataR )
	, ( "_allocDataRS",			primAllocDataRS )
	, ( "primStore_peekDataR_payload",	peekDataR_payload )
	, ( "primStore_peekDataRS_payload",	peekDataRS_payload )
	, ( "primArrayInt32_poke",		arrayUI_poke )
	, ( "primArrayInt32_peek",		arrayUI_peek )
	]


primAllocDataRS :: [Exp a] -> LlvmM LlvmVar
primAllocDataRS [ XLit (LLit (LiteralFmt (LInt tag) (UnboxedBits 32)))
		, XLit (LLit (LiteralFmt (LInt dataSize) (UnboxedBits 32))) ]
 = allocDataRSbySize (fromInteger tag) (fromInteger dataSize)

primAllocDataR :: [Exp a] -> LlvmM LlvmVar
primAllocDataR	[ XLit (LLit (LiteralFmt (LInt tag) (UnboxedBits 32)))
		, size@XVar{} ]
 = do 	dataSize	<- llvmOfExp size
	allocDataR	(fromInteger tag) dataSize



peekDataR_payload :: [Exp a] -> LlvmM LlvmVar
peekDataR_payload [exp]
 = do	addComment	"peekDataR_payload"
	addAlias	("struct.DataR", llvmTypeOfStruct ddcDataR)

	let offset	= fst $ structFieldLookup ddcDataR "payload"
	pdata		<- newUniqueNamedReg "pDataR" pStructDataR
	ptr		<- newUniqueNamedReg "pVoid" pChar

	pobj		<- llvmOfExp exp
	addBlock	[ Assignment pdata (Cast LM_Bitcast pobj pStructDataR )
			, Assignment ptr (GetElemPtr True pdata [ i32LitVar 0, i32LitVar offset, i32LitVar 0 ])
			]
	return		ptr


peekDataRS_payload :: [Exp a] -> LlvmM LlvmVar
peekDataRS_payload [ exp@(XVar _ t) ]
 = do	addComment	"peekDataRS_payload"
	addAlias	("struct.DataRS", llvmTypeOfStruct ddcDataRS)

	let offset	= fst $ structFieldLookup ddcDataRS "payload"
	pdata		<- newUniqueNamedReg "pDataRS" pStructDataRS
	ptr		<- newUniqueNamedReg "pVoid" pChar

	pobj		<- llvmOfExp exp
	addBlock	[ Assignment pdata (Cast LM_Bitcast pobj pStructDataRS )
			, Assignment ptr (GetElemPtr True pdata [ i32LitVar 0, i32LitVar offset, i32LitVar 0 ])
			]
	return		ptr

--------------------------------------------------------------------------------

arrayUI_peek :: [Exp a] -> LlvmM LlvmVar
arrayUI_peek	[ array@XVar{}
		, index@XVar{}
		]
 = do	addComment	"arrayUI_peek"
	addAlias	("struct.DataAUI", llvmTypeOfStruct ddcDataAUI)

	let ploff	= fst $ structFieldLookup ddcDataAUI "elem"
	pDataAUI	<- newUniqueNamedReg "arrray" pStructDataAUI
	pindex		<- newUniqueNamedReg "index" pInt32
	result		<- newUniqueNamedReg "result" i32

	pobj		<- llvmOfExp array
	ix		<- llvmOfExp index
	addBlock	[ Assignment pDataAUI (Cast LM_Bitcast pobj pStructDataAUI )
			, Assignment pindex (GetElemPtr True pDataAUI [ i32LitVar 0, i32LitVar ploff, ix ])
			, Assignment result (Load pindex)
			]
	return		result


arrayUI_poke :: [Exp a] -> LlvmM LlvmVar
arrayUI_poke	[ array@XVar{}
		, index@XVar{}
		, value@XVar{}
		]
 = do	addComment	"arrayUI_poke"
	addAlias	("struct.DataAUI", llvmTypeOfStruct ddcDataAUI)

	let ploff	= fst $ structFieldLookup ddcDataAUI "elem"
	pDataAUI	<- newUniqueNamedReg "arrray" pStructDataAUI
	pindex		<- newUniqueNamedReg "index" pInt32

	pobj		<- llvmOfExp array
	ix		<- llvmOfExp index
	ivalue		<- llvmOfExp value
	addBlock	[ Assignment pDataAUI (Cast LM_Bitcast pobj pStructDataAUI )
			, Assignment pindex (GetElemPtr True pDataAUI [ i32LitVar 0, i32LitVar ploff, ix ])
			, Store ivalue pindex
			]
	addComment	"arrayUI_poke end"
	return		ivalue
