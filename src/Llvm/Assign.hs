{-# OPTIONS -fno-warn-unused-binds -fno-warn-type-defaults -cpp #-}

module Llvm.Assign
	(llvmOfAssign)
where

import DDC.Base.DataFormat
import DDC.Base.Literal
import DDC.Main.Error
import DDC.Sea.Exp
import DDC.Sea.Pretty

import Llvm
import LlvmM
import Llvm.Runtime
import Llvm.Util
import Llvm.XPrim
import Llvm.Var


stage = "Llvm.Assign"

debug = True

llvmOfAssign :: Exp a -> Type -> Exp a -> LlvmM ()

-- Special case NULL pointer assignment
llvmOfAssign (XVar v1@NCafPtr{} t1) t@(TPtr (TCon TyConObj)) (XLit (LLit (LiteralFmt (LInt 0) Unboxed)))
 | t1 == t
 = do	dst		<- newUniqueReg $ pLift $ toLlvmType t1
	addBlock	[ Assignment dst (loadAddress (pVarLift (toLlvmCafVar (varOfName v1) t1)))
			, Store (LMLitVar (LMNullLit (toLlvmType t1))) dst ]

llvmOfAssign dst@(XVar n@NAuto{} t@(TPtr (TCon TyConObj))) tc src
 | t == tc
 = do	reg		<- evalSrc tc src
	alloc		<- newNamedReg (seaVar True $ varOfName n) $ toLlvmType t
	addBlock	[ Assignment alloc (Alloca pObj 1)
			, Store reg (pVarLift alloc) ]

llvmOfAssign (XVar (NSlot v i) tv@(TPtr (TCon TyConObj))) tc src
 | tv == tc
 = do	reg	<- evalSrc tc src
	writeSlot reg i

llvmOfAssign (XVar v1@NCaf{} tv@(TPtr (TPtr (TCon TyConObj)))) tc src
 | tv == tc
 = do	reg		<- evalSrc tc src
	addBlock	[ Store reg (pVarLift (toLlvmCafVar (varOfName v1) tv)) ]

llvmOfAssign (XVar v1@NCafPtr{} tv@(TPtr (TCon TyConObj))) tc src
 | tv == tc
 = do	reg		<- evalSrc tc src
	dest		<- newUniqueReg $ toLlvmType tv
	addBlock	[ Assignment dest (loadAddress (pVarLift (toLlvmCafVar (varOfName v1) tv)))
			, Store reg (pVarLift dest) ]

llvmOfAssign (XVar v@NRts{} tv) tc src
 = do	reg		<- evalSrc tc src
	addBlock	[ Store reg (pVarLift (toLlvmRtsVar (varOfName v) tv)) ]



llvmOfAssign a b c
 = panic stage $ "llvmOfAssign (" ++ (show __LINE__) ++ ") Unhandled : \n"
	++ {- take 150 -} (show a) ++ "\n"
	++ {- take 150 -} (show b) ++ "\n"
	++ {- take 150 -} (show c) ++ "\n"

--------------------------------------------------------------------------------

evalSrc :: Type -> Exp a -> LlvmM LlvmVar
evalSrc tc (XVar v2@NRts{} t2@(TPtr (TPtr (TCon TyConObj))))
 | tc == t2
 = do	reg		<- newUniqueReg $ toLlvmType tc
	addBlock	[ Assignment reg (loadAddress (toLlvmRtsVar (varOfName v2) t2)) ]
	return		reg

evalSrc tc (XVar (NSlot _ n) tv)
 | tc == tv
 =	readSlot n

evalSrc tc (XVar n@NAuto{} tv)
 | tc == tv
 =	return $ toLlvmVar (varOfName n) tc

evalSrc t@(TPtr (TCon TyConObj)) (XPrim op args)
 =	llvmOfXPrim op args




evalSrc tc (XVar n@NSuper{} tv)
 | tc == tv
 = panic stage $ "evalSrc (" ++ (show __LINE__) ++ ") :\n"
	++ show tc ++ "\n"
	++ show n ++ "\n"

evalSrc tc (XVar n@NCafPtr{} tv)
 | tc == tv
 = panic stage $ "evalSrc (" ++ (show __LINE__) ++ ") :\n"
	++ show tc ++ "\n"
	++ show n ++ "\n"

evalSrc tc (XVar n@NCaf{} tv)
 | tc == tv
 = panic stage $ "evalSrc (" ++ (show __LINE__) ++ ") :\n"
	++ show tc ++ "\n"
	++ show n ++ "\n"



evalSrc tc src
 = panic stage $ "evalSrc (" ++ (show __LINE__) ++ ") :\n"
	++ show tc ++ "\n"
	++ show src ++ "\n"

