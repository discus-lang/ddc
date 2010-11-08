{-# OPTIONS -fno-warn-unused-binds -fno-warn-type-defaults -cpp #-}

module Llvm.Assign
	(llvmOfAssign)
where

import DDC.Base.DataFormat
import DDC.Base.Literal
import DDC.Main.Error
import DDC.Sea.Exp

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



llvmOfAssign dst@(XVar n1@NAuto{} t1@(TPtr (TCon TyConObj))) tc src
 | t1 == tc
 = do	reg	<- evalSrc tc src
	addBlock [ Assignment (toLlvmVar (varOfName n1) tc) (loadAddress reg) ]

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
	addBlock	[ Store reg (pVarLift (toLlvmCafVar (varOfName v1) tv)) ]

llvmOfAssign xv@(XVar v1@NRts{} t1) tc src
 = do	reg		<- evalSrc tc src
	addBlock	[ Store reg (pVarLift (llvmVarOfXVar xv)) ]



llvmOfAssign a b c
 = panic stage $ "llvmOfAssign (" ++ (show __LINE__) ++ ") Unhandled : \n"
	++ {- take 150 -} (show a) ++ "\n"
	++ {- take 150 -} (show b) ++ "\n"
	++ {- take 150 -} (show c) ++ "\n"

--------------------------------------------------------------------------------

evalSrc :: Type -> Exp a -> LlvmM LlvmVar
evalSrc t (XVar v2@NRts{} t2)
 | t == t2
 = do	src		<- newUniqueReg $ toLlvmType t
	addBlock [ Assignment src (loadAddress (toLlvmCafVar (varOfName v2) t2)) ]
	return src

evalSrc t (XVar (NSlot _ 0) tv)
 | t == tv
 = do	reg		<- newUniqueNamedReg "slot.0" $ toLlvmType t
	addBlock	[ Assignment reg (Load localSlotBase) ]
	return		reg

evalSrc t (XVar (NSlot _ n) tv)
 | t == tv
 , n > 0
 = do	reg		<- newUniqueNamedReg ("slot." ++ show n) $ toLlvmType t
	addBlock	[ Assignment reg (GetElemPtr True localSlotBase [llvmWordLitVar n]) ]
	return		reg

evalSrc t (XVar n@NAuto{} tv)
 | t == tv
 = return $ toLlvmVar (varOfName n) t

evalSrc t@(TPtr (TCon TyConObj)) (XPrim op args)
 =	llvmOfXPrim op args




evalSrc t (XVar n@NSuper{} tv)
 | t == tv
 = panic stage $ "evalSrc (" ++ (show __LINE__) ++ ") :\n"
	++ show t ++ "\n"
	++ show n ++ "\n"

evalSrc t (XVar n@NCafPtr{} tv)
 | t == tv
 = panic stage $ "evalSrc (" ++ (show __LINE__) ++ ") :\n"
	++ show t ++ "\n"
	++ show n ++ "\n"

evalSrc t (XVar n@NCaf{} tv)
 | t == tv
 = panic stage $ "evalSrc (" ++ (show __LINE__) ++ ") :\n"
	++ show t ++ "\n"
	++ show n ++ "\n"



evalSrc t src
 = panic stage $ "evalSrc (" ++ (show __LINE__) ++ ") :\n"
	++ show t ++ "\n"
	++ show src ++ "\n"

