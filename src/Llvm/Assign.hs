{-# OPTIONS -fno-warn-unused-binds -fno-warn-type-defaults -cpp #-}

module Llvm.Assign
	(llvmOfAssign)
where

import DDC.Base.DataFormat
import DDC.Base.Literal
import DDC.Main.Error
import DDC.Sea.Exp
import DDC.Var

import Llvm
import LlvmM
import Llvm.Runtime
import Llvm.Util
import Llvm.Var
import Llvm.Exp


stage = "Llvm.Assign"

debug = True

llvmOfAssign :: Exp a -> Type -> Exp a -> LlvmM ()

-- Special case NULL pointer assignment
llvmOfAssign (XVar v1@NCafPtr{} t1) t@(TPtr (TCon TyConObj)) (XLit (LLit (LiteralFmt (LInt 0) Unboxed)))
 | t1 == t
 = do	dst		<- newUniqueReg $ pLift $ toLlvmType t1
	cv		<- toLlvmCafVar (varOfName v1) t1
	addComment	$ "Assigning (LInt 0) to a pointer!!"
	addBlock	[ Assignment dst (loadAddress (pVarLift cv))
			, Store (LMLitVar (LMNullLit (toLlvmType t1))) dst ]

llvmOfAssign dst typ (XLit LNull)
 =	assignNull dst typ


llvmOfAssign (XVar (NAuto v) t) tc src
 | t == tc
 = do	reg		<- llvmOfExp src
	addBlock	[ Store reg (pVarLift (toLlvmVar v t)) ]

llvmOfAssign (XVar (NSlot v i) tv@(TPtr (TCon TyConObj))) tc src
 | tv == tc
 = do	reg		<- llvmOfExp src
	writeSlot	reg i

llvmOfAssign (XVar v1@NCaf{} tv@(TPtr (TPtr (TCon TyConObj)))) tc src
 | tv == tc
 = do	reg		<- llvmOfExp src
	cv		<- toLlvmCafVar (varOfName v1) tv
	addBlock	[ Store reg (pVarLift cv) ]

llvmOfAssign (XVar v1@NCafPtr{} tv@(TPtr (TCon TyConObj))) tc src
 | tv == tc
 = do	reg		<- llvmOfExp src
	dest		<- newUniqueReg $ toLlvmType tv
	cv		<- toLlvmCafVar (varOfName v1) tv
	addBlock	[ Assignment dest (loadAddress (pVarLift cv))
			, Store reg (pVarLift dest) ]

llvmOfAssign (XVar v@NRts{} tv) tc src
 = do	reg		<- llvmOfExp src
	addBlock	[ Store reg (pVarLift (toLlvmRtsVar (varOfName v) tv)) ]

llvmOfAssign (XArgThunk (XVar (NSlot _ ix) tv@(TPtr (TCon TyConObj))) i) tc src
 = do	rsrc		<- llvmOfExp src
	obj		<- readSlot ix
	thunk		<- newUniqueReg $ pStructThunk
	let indx	= fst $ structFieldLookup ddcThunk "args"
	ptr		<- newUniqueReg $ pObj

	addBlock	[ Assignment thunk (Cast LM_Bitcast obj pStructThunk)
			, Assignment ptr (GetElemPtr True thunk [ i32LitVar 0, i32LitVar indx, i32LitVar i ])
			, Store rsrc (pVarLift ptr) ]

llvmOfAssign (XArgData (XVar (NSlot _ ix) tv@(TPtr (TCon TyConObj))) i) tc src
 = do	rsrc		<- llvmOfExp src
	obj		<- readSlot ix
	pdata		<- newUniqueReg $ pStructData
	let indx	= fst $ structFieldLookup ddcData "args"
	ptr		<- newUniqueReg $ pObj

	addBlock	[ Assignment pdata (Cast LM_Bitcast obj pStructData)
			, Assignment ptr (GetElemPtr True pdata [ i32LitVar 0, i32LitVar indx, i32LitVar i ])
			, Store rsrc (pVarLift ptr) ]


llvmOfAssign (XVar v1@NCaf{} tv) tc@(TCon (TyConUnboxed _)) src
 | tv == tc
 = do	reg		<- llvmOfExp src
	cv		<- toLlvmCafVar (varOfName v1) tv
	addBlock	[ Store reg (pVarLift cv) ]


llvmOfAssign a b c
 = panic stage $ "llvmOfAssign (" ++ (show __LINE__) ++ ") Unhandled : \n\n"
	++ {- take 150 -} (show a) ++ "\n\n"
	++ {- take 150 -} (show b) ++ "\n\n"
	++ {- take 150 -} (show c) ++ "\n"

--------------------------------------------------------------------------------

assignNull :: Exp a -> Type -> LlvmM ()
assignNull (XVar (NSlot v i) tv@(TPtr _)) (TPtr _)
 =	writeSlot	(LMLitVar (LMNullLit (toLlvmType tv))) i

assignNull (XVar v@NCafPtr{} t@(TPtr _)) (TPtr _)
 = do	dst		<- newUniqueReg $ pLift $ toLlvmType t
	cv		<- toLlvmCafVar (varOfName v) t
	addBlock	[ Assignment dst (loadAddress (pVarLift cv))
			, Store (LMLitVar (LMNullLit (toLlvmType t))) dst ]

assignNull (XVar v@NCaf{} t@(TPtr _)) (TPtr _)
 = do	dst		<- newUniqueReg $ pLift $ toLlvmType t
	cv		<- toLlvmCafVar (varOfName v) t
	addBlock	[ Assignment dst (loadAddress (pVarLift cv))
			, Store (LMLitVar (LMNullLit (toLlvmType t))) dst ]

assignNull (XVar (NAuto v) t) tc@(TCon (TyConUnboxed tv))
 | t == tc
 =	let zero = case varName tv of
			"Int32#"	-> i32LitVar 0
			"Bool#"		-> LMLitVar (LMIntLit (toInteger 0) i1)

	in addBlock	[ Store zero (pVarLift (toLlvmVar v t)) ]


assignNull xv t
 = panic stage $ "assignNull (" ++ (show __LINE__) ++ ") Unhandled : \n\n"
	++ show xv ++ "\n\n"
	++ show t ++ "\n\n"
